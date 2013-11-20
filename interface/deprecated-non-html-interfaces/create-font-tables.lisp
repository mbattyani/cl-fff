(in-package interface)

(fli:define-foreign-function (%win-exec "WinExec") ((lpcmdline :pchar) (ucmdshow :ulong))
 :language :c
 :calling-convention :stdcall
 :result-type :long
 :module "kernel32.dll")

(defun win-exec (command) 
  (fli:with-dynamic-foreign-objects
   ()
   (%win-exec (fli:convert-to-dynamic-foreign-string command :external-format win32:*multibyte-code-page-ef*) 1)))

(fli:define-c-typedef :hfont :handle)
(fli:define-c-typedef :hdc :handle)
(fli:define-c-typedef :hwnd :handle)

(fli:define-foreign-function (delete-object "DeleteObject") ((A0 :handle))
 :language :c
 :calling-convention :stdcall
 :result-type (:boolean :long)
 :module "gdi32.dll")

(fli:define-foreign-function (%create-font "CreateFontA") ((A0 :long) (A1 :long) (A2 :long) (A3 :long) (A4 :long) (A5 :ulong) (A6 :ulong) (A7 :ulong) (A8 :ulong) (A9 :ulong) (A10 :ulong) (A11 :ulong) (A12 :ulong) (A13 :pchar))
 :language :c
 :calling-convention :stdcall
 :result-type :hfont
 :module "gdi32.dll")

(fli:define-foreign-function (get-dc "GetDC") ((hwnd :hwnd))
 :language :c
 :calling-convention :stdcall
 :result-type :hdc
 :module "user32.dll")

(fli:define-foreign-function (release-dc "ReleaseDC") ((hwnd :hwnd) (hdc :hdc))
 :language :c
 :calling-convention :stdcall
 :result-type :long
 :module "user32.dll")

(fli:define-c-struct :size
  (cx :long)
  (cy :long)
  )
(fli:define-c-typedef :lpsize (:pointer :size))

(fli:define-foreign-function (get-text-extent-point32 "GetTextExtentPoint32A") ((A0 :hdc) (A1 :pchar) (A2 :long) (A3 :lpsize))
 :language :c
 :calling-convention :stdcall
 :result-type (:boolean :long)
 :module "gdi32.dll")

(defun %text-size (hDC text)
  (fli:with-dynamic-foreign-objects ()
     (let ((text-size (fli:allocate-dynamic-foreign-object :type :size)))
       (multiple-value-bind (str length) (fli:convert-to-dynamic-foreign-string text :external-format win32:*multibyte-code-page-ef*)
			    (get-text-extent-point32 hDC str (1- length) text-size)
			    (values (fli:foreign-slot-value text-size 'cx) (fli:foreign-slot-value text-size 'cy))))))

(fli:define-foreign-function (select-object "SelectObject") ((A0 :hdc) (A1 :handle))
 :language :c
 :calling-convention :stdcall
 :result-type :handle
 :module "gdi32.dll")

;;************************************************************************************

(defvar *gdi-objects-allocated* 0)

(defclass gdi-object ()
 ((handle :initform 0 :accessor handle :initarg :handle)
  (dont-delete :initform () :accessor dont-delete)
  ))

(defmethod initialize-instance :after ((gdi-obj gdi-object) &rest init-options &key &allow-other-keys)
  (declare (ignore init-options))
  (unless (dont-delete gdi-obj)
	  (incf *gdi-objects-allocated*)
	  (hcl:flag-special-free-action gdi-obj)))

(defun delete-gdi-object (gdi-obj)
  (unless (dont-delete gdi-obj)
	  (hcl:flag-not-special-free-action gdi-obj)
	  (decf *gdi-objects-allocated*)
	  (delete-object (handle gdi-obj))
	  (setf (handle gdi-obj) 0)))

(defun unflag-gdi-object (gdi-obj)
  (hcl:flag-not-special-free-action gdi-obj)
  (decf *gdi-objects-allocated*))

(defun %free-gdi-object (object)
  (when (typep object 'gdi-object)
;	(print object)
	(decf *gdi-objects-allocated*)
	(delete-object (handle object))))

(hcl:add-special-free-action '%free-gdi-object)

(defclass win-font-object (gdi-object)
 ((face-name :initform "arial" :accessor face-name :initarg :face-name)
  (height :initform -10 :accessor height :initarg :height)
  (orientation :initform 0 :accessor orientation :initarg :orientation)
  (weight :initform 400 :accessor weight :initarg :weight)
  (italic :initform () :accessor italic :initarg :italic)
  (underline :initform () :accessor underline :initarg :underline)
  ))

(defmethod initialize-instance :after ((font-obj win-font-object) &rest init-options &key bold &allow-other-keys)
  (declare (ignore init-options))
  (when bold (setf (weight font-obj) 800))
  (fli:with-foreign-string (str element-count byte-count :external-format win32:*multibyte-code-page-ef*) (face-name font-obj)
			   (setf (handle font-obj) (%create-font (height font-obj) 0 (orientation font-obj) (orientation font-obj)
								 (weight font-obj) (if (italic font-obj) 1 0)
								 (if (underline font-obj) 1 0) 0 0 0 0 0 0 str))))

(defvar *win-control-default-font* (make-instance 'win-font-object :height -12 :face-name "arial"))

(defun win-text-size (text &optional (font *win-control-default-font*))
  (let ((old-font nil)
	(dc (get-dc 0)))
    (when font (setf old-font (select-object dc (handle font))))
    (multiple-value-prog1 (%text-size dc text)
			  (when old-font (select-object dc old-font))
			  (release-dc 0 dc))))

(defvar *std-font-sizes* '(7 8 9 10 11 12 13 14 15 16 18 20 24 28 32 36))

(defun add-size (s size font)
  (format s "(make-array 256  :element-type '(unsigned-byte 8) :initial-contents '~s)~%"
	  (loop with str = " "
		for i from 0 to 255
		do (setf (aref str 0) (code-char i))
		collect (win-text-size str font))))
 
(defun write-font-table (font-names filename)
  (with-open-file (s "font-tables.lisp" :direction :output :if-exists :supersede)
    (format s "(in-package interface)~%")
    (dolist (name font-names)
      (format s "(let ((f (make-instance 'font-table :face-name ~s)))~%" name)
      (format s "(setf (gethash ~s *font-tables*) f)~%" name)
      (dolist (size *std-font-sizes*)
	(let ((font (make-instance 'win-font-object :face-name name :height (- size))))
	  (format s "(setf (gethash ~a (sizes f)) (list ~%" size)
	  (multiple-value-bind (dx dy) (win-text-size "A")
	    (format s "~d ~%" dy))
	  (add-size s size font)
	  (setf font (make-instance 'win-font-object :face-name name :height (- size) :bold t))
	  (add-size s size font)
	  (setf font (make-instance 'win-font-object :face-name name :height (- size) :italic t))
	  (add-size s size font)
	  (setf font (make-instance 'win-font-object :face-name name :height (- size) :italic t :bold t))
	  (add-size s size font)
	  (format s "))~%" )))
      (format s ")~%~%" ))))

      