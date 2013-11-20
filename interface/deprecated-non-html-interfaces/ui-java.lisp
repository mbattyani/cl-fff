(in-package layout)

(defun parent-name (parent)
  (if parent 
      (name parent)
   "getContentPane()"))

(defmethod write-construction :after ((item ui-item) (container (eql nil)) (language (eql :java)))
  (format *layout-stream*
"  getContentPane().add(~A);
   ~A.setBounds(~A, ~A, ~A, ~A);
" (name item)(name item) (round (x item))(round (y item))(round (dx item))(round (dy item))))

(defmethod write-construction :after ((item ui-item) container (language (eql :java)))
  (when (tooltip item) (format *layout-stream* "  ~A.setToolTipText(~S);~%" (name item)(tooltip item))))

(defmethod write-destruction (item container (language (eql :java)))
  (dolist (sub-item (sub-items item))
     (format *layout-stream* "
  if (~A != null)
    {
     ~A.remove(~A);
     ~A = null;
    }
" (name sub-item)(name item)(name sub-item)(name sub-item))))

(defmethod write-declaration :after ((ui-container ui-container) container (language (eql :java)))
  (dolist (item (sub-items ui-container))
	  (write-declaration item ui-container :java)))

(defmethod write-declaration ((item panel) container (language (eql :java)))
  (format *layout-stream* "  JPanel ~A;~%" (name item)))

(defmethod write-construction ((item panel) container (language (eql :java)))
  (format *layout-stream* "  ~A = new JPanel(null);~%" (name item))
  (when (border item)
	(format *layout-stream* "  {Border border = BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder(), ~S);~A.setBorder(border);}~%" (text item)(name item)))
  (dolist (sub-item (sub-items item))
	  (write-construction sub-item item :java)
	  (format *layout-stream* "  ~A.add(~A);~%  ~A.setBounds(~A, ~A, ~A, ~A);~%"
		  (name item)(name sub-item) (name sub-item)(round (x sub-item))(round (y sub-item))(round (dx sub-item))(round (dy sub-item)))))

(defmethod write-declaration ((item tabbed-pane) container (language (eql :java)))
  (format *layout-stream* "  JTabbedPane ~A;~%" (name item)))

(defmethod write-construction ((item tabbed-pane) container (language (eql :java)))
  (format *layout-stream* "  ~A = new JTabbedPane();~%//  ~A.setLayout(null);~%" (name item)(name item))
  (loop for tab-item in (tab-items item)
	as sub-item = (item tab-item)
	do
	(write-construction sub-item item :java)
	(format *layout-stream* "  ~A.add(~S, ~A);~%  ~A.setBounds(~A, ~A, ~A, ~A);~%"
		(name item)(text tab-item)(name sub-item) (name sub-item)(round (x sub-item))(round (y sub-item))(round (dx sub-item))(round (dy sub-item)))))

(defmethod write-declaration ((item label) container (language (eql :java)))
  (format *layout-stream* "  JLabel ~A;~%" (name item)))

(defmethod write-construction ((item label) container (language (eql :java)))
  (format *layout-stream* "  ~A = new JLabel(~S);~%" (name item)(text item)))

(defmethod write-declaration ((item push-button) container (language (eql :java)))
  (format *layout-stream* "  JButton ~A;~%" (name item)))

(defmethod write-construction ((item push-button) container (language (eql :java)))
  (format *layout-stream* "  ~A = new JButton(~S);~%" (name item)(text item)))

(defmethod write-declaration ((item radio-button) container (language (eql :java)))
  (format *layout-stream* "  JRadioButton ~A;~%" (name item)))

(defmethod write-construction ((item radio-button) container (language (eql :java)))
  (format *layout-stream* "  ~A = new JRadioButton(~S);~%" (name item)(text item)))

(defmethod write-declaration ((item check-box-button) container (language (eql :java)))
  (format *layout-stream* "  JCheckBox ~A;~%" (name item)))

(defmethod write-construction ((item check-box-button) container (language (eql :java)))
  (format *layout-stream* "  ~A = new JCheckBox(~S);~%" (name item)(text item)))

(defmethod write-declaration ((item edit) container (language (eql :java)))
  (format *layout-stream* "  JTextField ~A;~%" (name item)))

(defmethod write-construction ((item edit) container (language (eql :java)))
  (format *layout-stream* "  ~A = new JTextField();~%" (name item)))

(defmethod write-declaration ((item multi-line-edit) container (language (eql :java)))
  (format *layout-stream* "  JTextArea ~A;~%" (name item)))

(defmethod write-construction ((item multi-line-edit) container (language (eql :java)))
  (format *layout-stream* "  ~A = new JTextArea();~%   ~A.setLineWrap(true);" (name item)(name item)))

(defmethod write-declaration ((item combo-box) container (language (eql :java)))
  (format *layout-stream* "  JComboBox ~A;~%" (name item)))

(defmethod write-construction ((item combo-box) container (language (eql :java)))
  (format *layout-stream* "  ~A = new JComboBox();~%" (name item))
  (dolist (combo-item (combo-items item))
    (format *layout-stream* "  ~A.addItem(~S);~%" (name item)combo-item)))

(defmethod write-interface (item (language (eql :java)))
  (format *layout-stream* "
import java.awt.*;
import java.awt.event.*;
import java.net.*;
import java.applet.*;
import javax.swing.*;
import javax.swing.border.*;

public class ~A extends JApplet {
"name)
  (write-declaration  item nil :java)
  (format *layout-stream* "
    public void init() 
       {
	String laf = UIManager.getSystemLookAndFeelClassName();
	try 
           {
	    UIManager.setLookAndFeel(laf);
	   } 
        catch (UnsupportedLookAndFeelException exc) 
           {
	    System.err.println(\"Warning: UnsupportedLookAndFeel: \" + laf);
	   } 
        catch (Exception exc) 
           {
	    System.err.println(\"Error loading \" + laf + \": \" + exc);
	   }
")
  (write-construction item nil :java)
  (format *layout-stream* "
       }
    public void stop() 
       {
")  
  (write-destruction  item nil :java)
  (format *layout-stream* "
       }
}
"))
