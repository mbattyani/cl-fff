(in-package interface)

(defun parent-hWnd (item)
  (if (parent item)
    (format nil "~a.m_hWnd" (name (parent item)))
   "this->m_hWnd"))

(defmethod write-atl-define-id ((item ui-item) container)
  (format *layout-stream* "#define ~A_ID ~a~%" (name item)(id item)))

(defmethod write-atl-define-id :after ((ui-container ui-container) container)
  (dolist (item (sub-items ui-container))
	  (write-atl-define-id item ui-container)))

(defmethod write-atl-declaration :after ((ui-container ui-container) container)
  (dolist (item (sub-items ui-container))
	  (write-atl-declaration item ui-container)))

;****************** panel ***********************
(defmethod write-atl-declaration ((item panel) container)
  (format *layout-stream* "    CPaneWindow ~A;~%" (name item)))

(defmethod write-atl-construction ((item panel) container)
  (format *layout-stream* "    ~A.Create(~a, ~a, ~a, ~a, ~a, ~s, ~a, ~a);~%"
	  (name item)(parent-hWnd item)(round (x item))(round (y item))(round (dx item))(round (dy item))
	  (if (text item) (format nil " ~a "(text item)) "")(if (border item) "EDGE_ETCHED" "0")(if (border item) "BF_RECT" "0"))
  (dolist (sub-item (sub-items item))
    (write-atl-construction sub-item item)))

;***************** tab pane **********************
(defmethod write-atl-declaration ((item tabbed-pane) container)
  (format *layout-stream* "    TabPane ~A;~%" (name item)))

(defmethod write-atl-construction ((item tabbed-pane) container)
  (format *layout-stream* "    ~A.Create(~a, ~a, this, ~a, ~a, ~a, ~a, ~d);~%"
	  (name item)(id item)(parent-hWnd item)(round (x item))(round (y item))(round (dx item))(round (dy item))
	  (length (tab-items item)))
  (loop for tab-item in (tab-items item)
	for i from 0
	as sub-item = (item tab-item)
	do
	(write-atl-construction sub-item item)
	(format *layout-stream* "    ~A.InsertItem(~d, ~s, &~a);~%" (name item) i (text tab-item)(name sub-item)))
  (format *layout-stream* "    ~A.SetCurrentTab(0);~%" (name item)))

;***************** vtab buttons **********************
(defmethod write-atl-declaration ((item vtab-buttons) container)
  (format *layout-stream* "    TabPane ~A;~%" (name item)))

(defmethod write-atl-construction ((item vtab-buttons) container)
  (format *layout-stream* "    ~A.Create(~a, ~a, this, ~a, ~a, ~a, ~a, ~d, TCS_BUTTONS|TCS_FIXEDWIDTH|TCS_MULTILINE);~%"
	  (name item)(id item)(parent-hWnd item)(round (x item))(round (y item))(round (dx item))(round (dy item))
	  (length (tab-items item)))
  (format *layout-stream* "    ~A.SetItemWidth(~d);~%" (name item)(round (dx item)))
  (loop for (text pane-name) in (tab-items item)
	for i from 0
	do
	(format *layout-stream* "    ~A.InsertItem(~d, ~s, &~a);~%" (name item) i text pane-name))
  (format *layout-stream* "    ~A.SetCurrentTab(0);~%" (name item)))

;******** Label *********
(defmethod write-atl-declaration ((item label) container)
  (format *layout-stream* "    LabelItem ~A;~%" (name item)))

(defmethod write-atl-construction ((item label) container)
  (format *layout-stream* "    ~A.Create(~a, ~a, this, ~a, ~a, ~a, ~a, ~s);~%"
	  (name item)(id item)(parent-hWnd item)(round (x item))(round (y item))(round (dx item))(round (dy item))(text item)))

;******** Push button ***********
(defmethod write-atl-declaration ((item push-button) container)
  (format *layout-stream* "    ButtonItem ~A;~%" (name item)))

(defmethod write-atl-construction ((item push-button) container)
  (format *layout-stream* "    ~A.Create(~a, ~a, this, ~a, ~a, ~a, ~a, ~s);~%"
	  (name item)(id item)(parent-hWnd item)(round (x item))(round (y item))(round (dx item))(round (dy item))(text item)))

;********* Radio button ***********
(defmethod write-atl-declaration ((item radio-button) container)
  (format *layout-stream* "    ButtonItem ~A;~%" (name item)))

(defmethod write-atl-construction ((item radio-button) container)
  (let ((flags "BS_AUTORADIOBUTTON"))
    (when (push-like item) (setf flags (concatenate 'string flags "|BS_PUSHLIKE")))
    (format *layout-stream* "    ~A.Create(~a, ~a, this, ~a, ~a, ~a, ~a, ~s, ~a);~%"
	    (name item)(id item)(parent-hWnd item)(round (x item))(round (y item))(round (dx item))(round (dy item))(text item) flags)))

;********* Check box button *******
(defmethod write-atl-declaration ((item check-box-button) container)
  (format *layout-stream* "    ButtonItem ~A;~%" (name item)))

(defmethod write-atl-construction ((item check-box-button) container)
  (let ((flags "BS_AUTOCHECKBOX"))
    (when (push-like item) (setf flags (concatenate 'string flags "|BS_PUSHLIKE")))
    (format *layout-stream* "    ~A.Create(~a, ~a, this, ~a, ~a, ~a, ~a, ~s, ~a);~%"
	  (name item)(id item)(parent-hWnd item)(round (x item))(round (y item))(round (dx item))(round (dy item))(text item) flags)))

;********* Edit ***********
(defmethod write-atl-declaration ((item edit) container)
  (format *layout-stream* "    EditItem ~A;~%" (name item)))

(defmethod write-atl-construction ((item edit) container)
  (let ((flags "0"))
    (when (read-only item) (setf flags (concatenate 'string flags "|ES_READONLY")))
    (when (password item) (setf flags (concatenate 'string flags "|ES_PASSWORD")))
    (format *layout-stream* "    ~A.Create(~a, ~a, this, ~a, ~a, ~a, ~a, ~a);~%"
	    (name item)(id item)(parent-hWnd item)(round (x item))(round (y item))(round (dx item))(round (dy item)) flags)))

;********* ListBox ***********
(defmethod write-atl-declaration ((item list-box) container)
  (format *layout-stream* "    ListBoxItem ~A;~%" (name item)))

(defmethod write-atl-construction ((item list-box) container)
  (format *layout-stream* "    ~A.Create(~a, ~a, this, ~a, ~a, ~a, ~a);~%"
	  (name item)(id item)(parent-hWnd item)(round (x item))(round (y item))(round (dx item))(round (dy item))))

;********* Multiline edit *********
(defmethod write-atl-declaration ((item multi-line-edit) container)
;  (format *layout-stream* "  JTextArea ~A;~%" (name item))
  )

(defmethod write-atl-construction ((item multi-line-edit) container)
;  (format *layout-stream* "  ~A = new JTextArea();~%   ~A.setLineWrap(true);" (name item)(name item))
  )

;********* Combo box **********
(defmethod write-atl-declaration ((item combo-box) container)
  (format *layout-stream* "    ComboItem ~A;~%" (name item)))

(defmethod write-atl-construction ((item combo-box) container)
  (format *layout-stream* "    ~A.Create(~a, ~a, this, ~a, ~a, ~a, ~a);~%"
	  (name item)(id item)(parent-hWnd item)(round (x item))(round (y item))(round (dx item)) 200)
  (format *layout-stream* "    ~A.Combo.SetItemHeight(-1, ~a);~%" (name item)(round (- (dy item) 6)))
  (dolist (combo-item (combo-items item))
    (format *layout-stream* "    ~A.Combo.AddString(~S);~%" (name item) combo-item)))

(defmethod write-atl-interface (item name &optional (path "D:/Fractal Concept/Sources/Modules/Interface/TestATL"))
  (with-open-file (*layout-stream* (format nil "~a/~a.h" path name) :direction :output :if-exists :supersede)
    (format *layout-stream* "
#pragma once
#include \"resource.h\"
#include \"items.h\"
")

    (write-atl-define-id item nil)
    (format *layout-stream* "

class C~aDlg : public CDialogImpl<C~aDlg>, EventProcess {
public:
  C~aDlg() {}

//BEGIN_MSG_MAP(C~aDlg)
//  COMMAND_ID_HANDLER(IDCANCEL, OnCancel)
//  MESSAGE_HANDLER(WM_INITDIALOG, OnInitDialog)
//  REFLECT_NOTIFICATIONS()
//END_MSG_MAP()

  virtual void PostInitDialog(){};

  enum { IDD = IDD_DIALOG };
" name name name name)
  (write-atl-declaration item nil)
  (format *layout-stream* "
public:

")
  (format *layout-stream* "
  LRESULT OnInitDialog(UINT, WPARAM, LPARAM, BOOL&) {
    ResizeClient(~d, ~d);
    CenterWindow();
" (round (dx item))(round (dy item)))
  (write-atl-construction item nil)
  (format *layout-stream* "
  PostInitDialog();
  return 0;
  }
};
")))

(defun make-atl-interface (name x y dx dy description &key border text (path "D:/Fractal Concept/Sources/Modules/Interface/TestATL"))
  (let ((*top-level-item* (make-instance 'ui-root :border border :text text)))
    (setf (sub-panes *top-level-item*) (apply-layout description))
    (optimize-layouts *top-level-item*)
    (compute-layout *top-level-item* x y dx dy)
    (compute-tab-order *top-level-item*)
    (write-atl-interface *top-level-item* name path) 
    *top-level-item*))

'(make-atl-interface "Test" 0 0 350 500
  '(:v
    (:push-button "ok" :name "OKButton")
    (:push-button "Cancel" :name "CancelButton")
    (:push-button "beep" :name "BeepButton")
    (:h (:edit)(:push-button "read"))
    (:g " Group Box "
     (:v 
      (:label "salut mon gars"(:push-button "read"))
      (:label "hello" (:combo ("item1" "item2" "item3" "item4")))))
    (:h
     (:v (:push-button "button 11")(:push-button "button 12")(:push-button "button 13"))
     (:v (:push-button "button 21")(:push-button "button 22")(:push-button "button 23"))
     (:v (:push-button "big button 31")(:push-button "button 32")(:push-button "button 33")))
    (:space :y-max 1000.0 :y-elasticity 1000.0)
    (:h (:push-button "ok")(:push-button "cancel") :align :center)
    :align :fit))

'(make-atl-interface "Test" 0 0 350 500
  '(:v
    (:t (("Lisp" (:v 
		  (:label "Description" (:combo ("Génial" "Trop cool" "Fantastique")))
		  (:label "Qualités" (:combo ("Toutes" "voire plus")))))
	 ("C++" (:v 
		 (:label "Description" (:combo ("Macro-assembleur" "pénible" "rase moquette")))
		 (:label "Qualités" (:combo ("verbeux" "pas OO")))))
	 ("Buttons" (:h (:g "Column A" (:v (:push-button "button 11")(:push-button "button 12")(:push-button "button 13")))
			  (:g "Column B" (:v (:push-button "button 11")(:push-button "button 12")(:push-button "button 13"))) :align :fill))))
    (:label "Langage" (:combo ("Lisp" "CLOS" "MOP" "Assembler")))
    (:g "Lisp"
     (:v 
      (:label "Description" (:combo ("Génial" "Trop cool" "Fantastique")))
      (:label "Qualités" (:combo ("Toutes" "voire plus")))))
    (:g "C++"
     (:v 
      (:label "Description" (:combo ("Macro-assembleur" "pénible" "rase moquette")))
      (:label "Qualités" (:combo ("verbeux" "pas OO")))))
    (:h (:label "Data") (:edit)(:push-button "read value")(:push-button "write value"))
    (:g "Bouttons"
     (:h
     (:v (:push-button "button 11")(:push-button "button 12")(:push-button "button 13"))
     (:v (:push-button "button 21")(:push-button "button 22")(:push-button "button 23"))
     (:v (:push-button "big button 31")(:push-button "button 32")(:push-button "button 33"))
      :align :fit))
    (:space :y-max 1000.0 :y-elasticity 10000.0)
    (:h (:push-button "submit")(:push-button "cancel") :align :center)
    :align :fit))

'(make-atl-interface "Test" 0 0 350 500
  '(:v
    (:t (("Lisp" (:v 
		  (:label "Description" (:combo ("Génial" "Trop cool" "Fantastique")))
		  (:label "Qualités" (:combo ("Toutes" "voire plus")))))
	 ("C++" (:v 
		 (:label "Description" (:combo ("Macro-assembleur" "pénible" "rase moquette")))
		 (:label "Qualités" (:combo ("verbeux" "pas OO")))))
	 ("Buttons" (:h (:g "Column A" (:v (:push-button "button 11")(:push-button "button 12")(:push-button "button 13")))
			  (:g "Column B" (:v (:push-button "button 11")(:push-button "button 12")(:push-button "button 13"))) :align :fill))))
    (:h (:vt (("Lisp" "Gr1")("C++" "Gr2")("Buttons" "Gr3")))
     (:stack
      (:g "Lisp"
	  (:v 
	   (:label "Description" (:combo ("Génial" "Trop cool" "Fantastique")))
	   (:label "Qualités" (:combo ("Toutes" "voire plus"))))
	  :name "Gr1")
      (:g "C++"
	  (:v 
	   (:label "Description" (:combo ("Macro-assembleur" "pénible" "rase moquette")))
	   (:label "Qualités" (:combo ("verbeux" "pas OO"))))
	  :name "Gr2")
      (:g "Bouttons"
	  (:h
	   (:list-box :name "LB1")
	   (:v (:edit :read-only t)(:push-button "button 12")(:push-button "button 13"))
	   (:v (:push-button "button 21")(:push-button "button 22")(:push-button "button 23"))
	   (:v (:push-button "big button 31")(:push-button "button 32")(:push-button "button 33"))
	   :align :fit)
	  :name "Gr3")))
    (:space :y-max 1000.0 :y-elasticity 10000.0)
    (:h (:push-button "submit")(:push-button "cancel") :align :center)
    :align :fit))