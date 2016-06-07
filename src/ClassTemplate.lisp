;;http://www.aiai.ed.ac.uk/~jeff/clos-guide.html

(defclass ClassName ();;(Parent1 Parent2)
                      ((Field :accessor get-field
                             :initform 10
                             :initarg  :symbol))
  )

(MAKE-INSTANCE 'ClassName :symbol 100)
                      