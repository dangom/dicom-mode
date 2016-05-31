;;; dicom-mode.el --- Extend image-mode with some DICOM functionality.

;; Copyright (C) 2016 Daniel Gomez

;; Author: Daniel Gomez <d.gomez@posteo.org>
;; Version: 0.1.0
;; Keywords: Medical Imaging, DICOM
;; Requirements: Emacs 24.3 (with imagemagick), dcm2niix (BSD Lic.), dcmtk (BSD Lic.).

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To make image-mode play well with dicoms, add the following to your init file:

;;(setq image-file-name-extensions
;;    (append '("dcm" "ima") image-file-name-extensions))

;; And to activate dicom-mode automatically when opening DICOMs add:

;; (require 'dicom-mode)
;; (add-to-list 'auto-mode-alist
;;              '("\\.\\(dcm\\|DCM\\|ima\\|IMA\\)\\'" . (lambda ()
;;                                (image-mode)
;;                                (dicom-mode))))

;; Dicom-mode provides facilities to view and manipulate DICOM images from
;; within Emacs' image-mode.
;; These facilities include:

;; Dump the DICOM header in a buffer.
;; Create publication-like protocol description.
;; Export current DICOM to nifti
;; Export all DICOMs in folder to nifti (3D and 4D)
;; TODO: Find alternative method to extract SliceAccelFactor.
;; TODO: Add more support for private vendor tags.
;; TODO: Expand Dicom-> Nifti facilities (choose filename and output folder)
;; TODO: Let user customize which tags to export, and what text to generate.
;; TODO: Add support to GDCM and let user choose DICOM his own header dumping program.
;; TODO: Add support for DICOM anonymization.
;; TODO: Import DICOM (or exported nifti) into an inferior Matlab or Python process.
;; TODO: Enable opening file on external viewer.
;; TODO: Refactor and remove hardcoded tag numbers from within functions.

;;; Code:
;;(eval-when-compile (require 'cl-lib)) ;;

;; Recommended DICOM tags for publication, as per COBIDAS Report (OHBM)
;; http://www.humanbrainmapping.org/files/2016/COBIDASreport.pdf
;; And some Tags necessary to calculate further information.
;; *THE-DICOM-TAGS* does not include any private vendor tags. These are handled
;; separately.

(load "./dicom-tags.el") ;; All DICOM tags described on the DICOM Standard.

(defvar *dcm/the-dicom-tags*
  '(("0008,103e" . "SeriesDescription")
    ("0018,0020" . "ScanningSequence")
    ("0018,0080" . "RepetitionTime")
    ("0018,0081" . "EchoTime")
    ("0018,0050" . "SliceThickness")
    ("0028,0030" . "PixelSpacing")
    ("0018,0095" . "PixelBandwidth")
    ("0008,0022" . "AcquisitionDate")
    ("0008,0032" . "AcquisitionTime")
    ("0008,0070" . "Manufacturer")
    ("0008,1090" . "ManufacturerModelName")
    ("0018,0087" . "MagneticFieldStrength")
    ;;("0051,100b" . "InPlaneMatrixSize") ;; Siemens private
    ("0018,0088" . "SpacingBetweenSlices")
    ("0018,1314" . "FlipAngle")
    ;;("0051,100a" . "TotalAcqTime") ;; Siemens private
    ;;("0051,100e" . "SliceOrientation") ;; Siemens private
    ("0018,0089" . "NumberOfPhaseEncodingSteps")
    ("0018,0091" . "EchoTrainLength")
    ("0018,1312" . "PhaseEncodingDirection")
    ("0018,1310" . "AcquisitionMatrix")))

;; The following vars will be used in the protocol summary. They are not
;; standard DICOM tags, but either private or deducible from the protocol.
;; For their values as specified in the DICOM.
(defvar *dcm/receiving-coil-name* "ReceivingCoilName") ;; Siemens 0051,100f
(defvar *dcm/inplane-resolution* "InPlaneResolution");; 0051,100b
(defvar *dcm/number-of-slices* "NumberOfSlices")
(defvar *dcm/number-of-volumes* "NumberOfVolumes")
(defvar *dcm/slice-orientation* "SliceOrientation")
(defvar *dcm/slice-gap* "SliceGap")
(defvar *dcm/pe-direction* "PEDirection")
(defvar *dcm/total-acq-time* "TotalAcqTime");; Private 0051,100a
(defvar *dcm/slice-accel-factor* "SliceAccelFactor")
(defvar *dcm/inplane-accel-factor* "InPlaneAccelFactor")
(defvar *dcm/slice-timestamps* "SliceTimestamps") ;; Private 0019,1029

(defvar *dcm/the-dicom-summary*
  "  Data were acquired using a Manufacturer ManufacturerModelName
  MagneticFieldStrengthT scanner, with a ReceivingCoilName coil.
  Images were obtained with a ScanningSequence sequence with the following parameters: TE
  = EchoTimems, TR = RepetitionTimems, in-plane resolution =PixelSpacing mm^2,
  in-plane matrix size =AcquisitionMatrix, NumberOfSlices SliceThicknessmm thick
  slices with a slice gap of SliceGapmm (orientation: SliceOrientation), flip angle =
  FlipAngle, pixel bandwidth = PixelBandwidth Hz/px, phase encoding direction
  = PEDirection.
  The total acquisition time of all NumberOfVolumes volumes comprised TotalAcqTime, accelerated
  with an in-plane acceleration factor of InPlaneAccelFactor, and a slice
  acceleration factor of SliceAccelFactor.")

;;;;;;;;;;;;;;;;;;;;;;;;
;; Some general helper functions
;;;;;;;;;;;;;;;;;;;;;;;;

;; list -> alist
(defun dcm/alist-from-list (input-list)
  "Take items from INPUT-LIST, two-by-two, and create an alist."
  (defun iter-func (input-list alist)
    (if (null input-list)
        alist
      (let ((new-alist
             (append alist `((,(first input-list) . ,(second input-list))))))
        (iter-func (cddr input-list) new-alist))))
  (iter-func input-list '()))

;; string, alist -> string
(defun dcm/get-val-from-key (key alist)
  (cdr (assoc key alist)))

;; string -> string
(defun dcm/remove-brackets (str)
  "Search and remove brackets [] in STR."
  (replace-regexp-in-string (rx (or "[" "]")) "" str))

;; Tip from
;; emacs.stackexchange.com/questions/13708/replace-all-literal-substrings-from-a-list-in-a-string
;; string, alist -> string
(defun dcm/translate-with-alist (str alist)
  "Substitute in STR all keys in ALIST for their values."
  (replace-regexp-in-string
   (regexp-opt (mapcar 'car alist)) ;; Match the cars.
   (lambda (match) (cdr (assoc match alist))) str)) ;; Put the cdrs.

;;;;;;;;;;;;;;;;;;;;;;;;
;; Some helper functions to detect values not explicitly written in DICOMs.
;;;;;;;;;;;;;;;;;;;;;;;;

;;; number number -> number
(defun dcm/infer-partial-fourier (pe-steps pe-matrix-size)
  "Detects partial fourier by computing PE-STEPS / PE-MATRIX-SIZE."
  (/ pe-steps pe-matrix-size))

;;; list -> number
(defun dcm/infer-slice-accel-factor (slice-timestamps)
  "Dividing the number of timestamps by the number of unique timestamps in SLICE-TIMESTAMPS."
  (let ((total-slices (length slice-timestamps))
        (unique-slices (length (remove-duplicates slice-timestamps))))
    (/ total-slices unique-slices)))

;; number number -> number
(defun dcm/infer-inplane-accel-factor (pe-steps etl)
  "Detects inplane accel factor by dividing PE-STEPS/ETL."
  (/ pe-steps etl))

;; list -> number
(defun dcm/infer-slice-order (slice-timestamps)
  "Compares actual timestamps with sorted timestamps in SLICE-TIMESTAMPS."
  (let ((sorted-timestamps (sort (copy-sequence slice-timestamps) '<)))
    (cond ((equalp slice-timestamps sorted-timestamps)
           "ascending")
          ((equalp slice-timestamps (reverse sorted-timestamps))
           "descending")
          (t "interleaved"))))

;; number, number -> number
(defun dcm/get-slice-gap (slice-spacing slice-thickness)
  "Gap between slices in mm.  Difference between SLICE-SPACING and SLICE-THICKNESS."
  (- slice-spacing slice-thickness))

;; list -> number
(defun dcm/get-number-of-slices (slice-timestamps)
  "Assume that the length of SLICE-TIMESTAMPS indicates the number of slices."
  (length slice-timestamps))

;; string -> string
(defun dcm/infer-pe-direction (dicomtagvalue)
  "Experimental.  Convert info in InPlanePhaseEncodingDirection DICOMTAGVALUE."
  (cond ((equal dicomtagvalue "COL") "A-P")
        ((equal dicomtagvalue "ROW") "R-L")
        (t "")))

;; string -> string
(defun dcm/get-inplane-resolution (dicomtagvalue)
  "Fix display of inplane-resolution from DICOMTAGVALUE."
  (let ((px-spacing (split-string dicomtagvalue "\\\\")))
    (concat (first px-spacing) "x" (second px-spacing))))

;; string -> string
(defun dcm/get-inplane-matrixsize (dicomtagvalue)
  "Fix display of inplane-matrix-size from DICOMTAGVALUE."
  (message dicomtagvalue)
  (let ((mat-size (split-string dicomtagvalue "\\\\")))
    (concat (first mat-size) "x" (car (last mat-size)))))

;; () -> number
(defun dcm/infer-number-of-volumes ()
  "Assume that the number of volumes is the number of files in the directory."
  (length
   (directory-files default-directory nil
                    ;; Exclude all but .dcm and .ima files, of course.
                    (rx "." (or "dcm" "ima" "IMA") string-end))))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Dicom dumping facilities. Requires dcmdump from dcmtk on PATH.
;;;;;;;;;;;;;;;;;;;;;;;;

;; string, string -> string
(defun dcm/search-for-a-dicom-tag (tag-as-str dcmfile)
  "Search for a particular dicom TAG-AS-STR in the DCMFILE, and return its value."
  (last (split-string (shell-command-to-string (concat "dcmdump +P " tag-as-str " +L +T " dcmfile)))))


(defun dcm/dicom-tag-search ()
  "Search for a particular dicom tag of the currently displayed image."
  (interactive)
  (let ((dcmtag (completing-read "Search for a DICOM tag: " *dcm/all-std-dicom-tags*)))
    (print (dcm/search-for-a-dicom-tag (substring dcmtag  0 9) buffer-file-name))))

;; The slice timestamps are in tag (0019,1029)
;; string -> list of numbers
(defun dcm/get-slice-timestamps (dcmfile)
  "Parse Siemens private tag 0019,1029 from DCMFILE to get slice timestamps as a list of numbers."
  (let* ((timestamp-tag "0019,1029")
         (raw-timestamps (shell-command-to-string (concat "dcmdump +P " timestamp-tag " +L +T " dcmfile)))
         (timestamps
          ;; We know that the longest string contains the timestamps.
          (first (sort (split-string raw-timestamps)
                       ;;so we sort using a predicate that compares lengths.
                       #'(lambda (a b) (if (> (length a) (length b)) t nil))))))
    (mapcar 'string-to-number (split-string timestamps "\\\\"))))

;; string -> string
(defun dcm/generate-dcmdump-string (the-dicom-file)
  "Create dcmdump shell command to grab wanted dicom tags from THE-DICOM-FILE."
  (concat "dcmdump "
          (mapconcat (function (lambda (x) (format "+P %s" x))) ;; +P : search tag
                     (mapcar 'car *dcm/the-dicom-tags*)
                     " ")
          ;; +T pretty print tree
          " +T " the-dicom-file))

;; string -> ()  // creates buffer
(defun dcm/dump-dicom-header (dcmfile)
  "Read dicom header info from DCMFILE and write its to *dicom-header* buffer."
  (let ((dicominfo (shell-command-to-string (dcm/generate-dcmdump-string dcmfile))))
    (with-current-buffer (get-buffer-create "*dicom-header*")
      (erase-buffer)
      (insert dicominfo))))

;;string -> alist   // creates buffer
(defun dcm/dump-dicom-header-extra (dcmfile)
  "Add extra info to dcmdump from DCMFILE."
  (let* ((rawdcminfo
          (shell-command-to-string (dcm/generate-dcmdump-string dcmfile)))
         (dcminfo
          (dcm/alist-from-list (split-string (dcm/remove-brackets rawdcminfo)))))
    (defun grab-from-dcminfo (x) (string-to-number (dcm/get-val-from-key x dcminfo)))
    (defun get-tag (tag) (dcm/search-for-a-dicom-tag tag dcmfile))
    (let ((pe-steps (grab-from-dcminfo "NumberOfPhaseEncodingSteps"))
          (etl (grab-from-dcminfo "EchoTrainLength"))
          (slice-spacing (grab-from-dcminfo "SpacingBetweenSlices"))
          (slice-thickness (grab-from-dcminfo "SliceThickness"))
          (number-of-volumes (number-to-string (dcm/infer-number-of-volumes)))
          (slice-timestamps (dcm/get-slice-timestamps dcmfile))
          (total-acq-time (dcm/remove-brackets (car (get-tag "0051,100a"))))
          (slice-orientation (dcm/remove-brackets (car (get-tag "0051,100e"))))
          (pe-direction (dcm/infer-pe-direction (dcm/get-val-from-key "InPlanePhaseEncodingDirection" dcminfo))))
      (let((number-of-slices (number-to-string (dcm/get-number-of-slices slice-timestamps)))
           (slice-accel-factor (number-to-string (dcm/infer-slice-accel-factor slice-timestamps)))
           (inplane-accel-factor (number-to-string (dcm/infer-inplane-accel-factor pe-steps etl)))
           (slice-gap (number-to-string (/ (ftruncate (* 100 (dcm/get-slice-gap slice-spacing slice-thickness))) 100)))
           (slice-order (dcm/infer-slice-order slice-timestamps))
           (px-spacing (dcm/get-inplane-resolution (cdr (assoc "PixelSpacing" dcminfo))))
           (mat-size (dcm/get-inplane-matrixsize (cdr (assoc "AcquisitionMatrix" dcminfo)))))
        (setq dcminfo (append dcminfo `((,*dcm/number-of-volumes* . ,number-of-volumes))))
        (setq dcminfo (append dcminfo `((,*dcm/slice-orientation* . ,slice-orientation))))
        (setq dcminfo (append dcminfo `((,*dcm/slice-gap* . ,slice-gap))))
        (setq dcminfo (append dcminfo `((,*dcm/number-of-slices* . ,number-of-slices))))
        (setq dcminfo (append dcminfo `((,*dcm/total-acq-time* . ,total-acq-time))))
        (setq dcminfo (append dcminfo `((,*dcm/inplane-accel-factor* . ,inplane-accel-factor))))
        (setq dcminfo (append dcminfo `((,*dcm/pe-direction* . ,pe-direction))))
        (setq dcminfo (append dcminfo `((,*dcm/slice-accel-factor* . ,slice-accel-factor))))
        (setf (cdr (assoc "PixelSpacing" dcminfo)) px-spacing)
        (setf (cdr (assoc "AcquisitionMatrix" dcminfo)) mat-size)
        (with-current-buffer (get-buffer-create "*dicom-header*")
          (erase-buffer)
          (insert (dcm/translate-with-alist *dcm/the-dicom-summary* dcminfo))
          (fill-paragraph)
          (insert "\n\n\n")
          (insert rawdcminfo))))))


(defun dcm/dicom-header ()
  "Dump dicom-header on *dicom-header* buffer."
  (interactive)
  (dcm/dump-dicom-header-extra buffer-file-name)
  (message "Dicom-header information exported to buffer *dicom-header*"))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Dicom -> Nifti exporting facilities.
;;;;;;;;;;;;;;;;;;;;;;;;

;; &optional string -> ()
(defun dcm/dcm2nii (&optional output-folder)
  "Export DICOMs to 4D nifti and store the result in OUTPUT-FOLDER."
  (shell-command-to-string (concat "dcm2niix " default-directory)))

(defun dcm/export-to-nifti ()
  "Export all dicoms in current folder to 4D nifti."
  (interactive)
  (dcm/dcm2nii)
  (message "Dicom exported to 4D Nifti."))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor Mode definition and keybindings
;;;;;;;;;;;;;;;;;;;;;;;;

(defvar dicom-mode-map nil
  "Keymap for Emacs dicom minor mode.")

(setq dicom-mode-map (make-sparse-keymap))

(define-key dicom-mode-map (kbd "C-c C-d") #'dcm/dicom-header)
(define-key dicom-mode-map (kbd "C-c C-i") #'dcm/export-to-nifti)
(define-key dicom-mode-map (kbd "C-c C-s") #'dcm/dicom-tag-search)

;;;###autoload
(define-minor-mode dicom-mode
  "Add functionality to image-mode to read DICOM headers, search for DICOM tag values and
export DICOMs to Niftis. Dicom-mode also creates a small publication-like description of the protocol
used to acquire the image.

Currently depends on dcm2niix for export to NIFTI, and on dcmtk to extract DICOM information.
"
  nil "dcm" dicom-mode-map)


(provide 'dicom-mode)
;;; dicom-mode.el ends here
