
;; AND двух переменных (a b)
(defun and2 (a b)
	(cond (a b)
		  (NIL)
	)
)

;; OR двух переменных (a b)
(defun or2 (a b)
	(cond ((not a) b)
		  (T)
	)
)

;; IMPLICATION двух переменных (a b)
(defun impl (a b)
	(not (and (eql a T)
			  (eql b NIL)
		 )
	)	
)


;; переменная (param) заменяется на (value), 
;; если текущая переменная (variable) совпадает c (param)
(defun change (param value variable)
	(cond ((not (atom variable)) (apply_bind param value variable))
		  ((eql param variable) value)
		  (T variable)
	)
)

;; подстановка в выражении (body) вместо переменной (param)
;; значение (value)
(defun apply_bind (param value body)
	(mapcar #'(lambda (variable) (change param value variable)) 
			body)
)

;; каждую переменную списка (params) заменяем на соответствующее ей
;; значение (value) в выражении (body)
(defun bind (params values body)
	(cond ((null params) body)
		  ((null values) body)
		  (T (bind (cdr params)
		  		   (cdr values) 
		  		   (apply_bind (car params) (car values) body)
		  	 )
		  )
	)
)

;; создается нулевой вектор для всех переменных (variables)
(defun create_none_vector (variables)
	(cond ((null variables) NIL)
		  (T (cons NIL (create_none_vector (cdr variables))))
	)
)

;; проверка: корректно ли выражение (expr)
(defun check_valid_expr (expr variables)
	(check_valid_one_item (bind variables
								(create_none_vector variables)
								expr
						  )
	)
)

;; проверка: корректно ли выражение одно булевское выражение (expr)
;; *** (and T) некорректно
(defun check_valid_one_item (expr)
	(let ((func (car expr)))  (cond ((or (eql func 'and2)
										 (eql func 'or2)
										 (eql func 'impl)
								    ) (check2arg expr)
							  		)
									((eql func 'not) (check1arg expr))
									((and (or (eql func T)
											  (eql func NIL)
										  )
										  (null (cdr expr))
									 )
									)
									(T (terpri)
									   (princ "*** Error!: Function is")
									   (princ " non-computable ***")
									   (terpri)
									   NIL
									)
							  )
    )
)

;; проверка: ровно ли 1 аргумент у ф-ции, принимающей 1 аргумент
(defun check1arg (L)
	(cond ((null (cdr L)) (terpri)
					(princ "*** Error!: Function ")
					(princ (car L))
					(princ " need 1 argument. Get 0 args ***")
					(terpri)
					NIL
		  )
		  ((not (null (cddr L))) (terpri)
		  						 (princ "*** Error!: Function ")
								 (princ (car L))
							   	 (princ " need 1 argument. Get > 1 args ***")
							   	 (terpri)
								 NIL
		  )
		  (T (check_valid_one_item (cadr L)))
	)
)

;; проверка: ровно ли 2 аргумента у ф-ции, принимающей 2 аргумента
(defun check2arg (L)
	(cond ((null (cdr L)) (terpri)
						  (princ "*** Error!: Function ")
						  (princ (car L))
						  (princ " need 2 argument. Get 0 args ***")
						  (terpri)
						  NIL
		  )
		  ((null (cddr L)) (terpri)
		  				   (princ "*** Error!: Function ")
						   (princ (car L))
						   (princ " need 2 argument. Get 1 args ***")
						   (terpri)
						   NIL
		  )
		  ((not (null (cdddr L))) (terpri)
		  						  (princ "*** Error!: Function ")
								  (princ (car L))
								  (princ " need 2 argument. Get > 2 args ***")
								  (terpri)
								  NIL
		  )
		  (T (and (check_valid_one_item (cadr L))
		  		  (check_valid_one_item (caddr L))
		  	 )
		  )
	)
)

;; вычисление выражения (expr)
(defun calculate (expr)
	(cond ((null expr) NIL)
		  ((eql (car expr) T) T)
		  ((eql (car expr) NIL) NIL)
		  ; ((not (check_valid_one_item expr)) 'error)
		  ((eql (car expr) 'not) (funcall (car expr)
		  			  					  (calculate (cadr expr))
		  						 )
		  )
		  (T (funcall (car expr)
		  			  (calculate (cadr expr))
		  			  (calculate (caddr expr))
		  	 )
		  )
	)
)

;; T & (NIL & T v (NIL v -NIL)) -> NIL
; (print (calculate '(impl (and2 T (or2 (and2 NIL T) (or2 NIL (not NIL)))) NIL )))
; (print (calculate '(T)))

; (print (bind '(X Y) '(NIL T) '(and2 Y (or2 X Y))))
; (print (calculate (bind '(X Y) '(NIL T) '(and2 Y (or2 X Y)))))


;; подсчет количества переменных в списке (variables)
; (defun length (variables)
; 	(reduce #'(lambda (cnt var) (+ cnt 1)) variables :initial-value 0)
; )

;; создаем список всех возможных значений переменных (variables)
;; *** ((00) (01) (10) (11)) если передан список, 
;; содержащий 2 переменные
(defun create_vectors_values(variables)
	(cond ((null variables) '(NIL))
		  (T (let* ((cnt_variables (length variables))
		   			(next_vector (create_vectors_values (cdr variables)))
		 	 	   )
		 	 	   (append (mapcar #'(lambda (tail) (cons NIL tail)) 
		 	 	   					next_vector
	   					   )
		 	 	   		   (mapcar #'(lambda (tail) (cons T tail)) 
		 	 	   					next_vector 
	   					   )
		 	 	   )	
			 )
		  )
	)	
)

; (print (length '(a b c d)))
; (print (create_vectors_values '(a b c d)))

;; при заданных значениях (value) переменных (variables)
;; высчитывается значение выражения (expr); 
;; и если значение True, то возвращается список значений (value),
;; иначе NIL
(defun put_variable_if_value_is_true (variables expr values)
	(cond ((calculate (bind variables values expr)) values)
	)
)

; (print (put_variable_if_value_is_true '(X Y) '(and2 Y (or2 X Y)) '(NIL NIL)))

;; удаление из списка (L) все элементы NIL
(defun delete_NIL (L)
	(cond ((null L) NIL)
		  ((cond ((null (car L)) (delete_NIL (cdr L)))
		  		 (T (cons (car L)
		  		 		  (delete_NIL (cdr L))
		  		 	)
		  		 )
		  ))
	)
)

;; составление списка всевозможных значений переменных (variables)
;; при которых выражение (expr) становится верным
(defun FAL_to_SDNF (variables expr)
	(sdnf (delete_NIL (mapcar #'(lambda (values) (put_variable_if_value_is_true variables
															  			  		expr
															  			  		values
										   		 )
						  		) 
								(create_vectors_values variables)
					  )
		  )
	)
)

; (print (FAL_to_SDNF '(X Y) '(and2 Y (or2 (not X) Y))))

;; подсчет значений TRUE в списке
(defun cnt_true(vector)
	(length (delete_NIL vector))
)

;; если (a b) разные -> T
;; если (a b) однинаковые -> NIL
(defun isdifferent (a b)
	(not (eql a b))
)

;; если (a) либо пустое, либо (a) == (b) -> T
(defun in1 (a b)
	(or (eql a '_) (eql a b))
)

;; если vector (a) целиком в (b) -> T
(defun in (a b)
	(reduce 'and2
			(mapcar #'(lambda (x y) (in1 x y))
		    		a
		    		b
			)
			:initial-value T
	)
)

;; составление списка различных элементов векторов (v1 v2)
;; *** (0 1 1 0) (0 0 _ 0) -> (NIL T T NIL)
(defun differences_2_vectors(v1 v2)
	(mapcar #'(lambda (a b) (isdifferent a b))
			v1
			v2
	)			 
)

;; по заданному вектору значений и булевскому векору различных позиций
;; построение вектора пересечение
;; *** (1 0 0 1) + (NIL T NIL NIL) -> (1 _ 0 1)
(defun get_intersection (vector differences)
	(mapcar #'(lambda (a b) (cond (b '_)
								  (T a)
							)
			  ) 
			vector
			differences 
	)
)

;; сокращение двух переменных (a b), если это возможно
(defun reduction_2_variables (v1 v2)
	(let* ((differences (differences_2_vectors v1 v2))
		   (cnt_differences (cnt_true differences))
		  )
		  (cond ((eql cnt_differences 1) (get_intersection v1 differences))   
		  		((eql cnt_differences 0) NIL)
		  		((in v1 v2) v1)
		  		((in v2 v1) v2)
		  )
	)
)

;; сокращение списка (list) с переменной (variable)
(defun reduction_variable_and_list (variable dnf)
	(let ((reductions (delete_NIL (mapcar #'(lambda (v2) (reduction_2_variables variable v2))
											 dnf
						 		  )
					  )
		  )
		 )
		 (cond ((eql (length reductions) 0) (list variable))
		 	   (T reductions)
		 )
	)
)

; (print (reduction_variable_and_list '(T T NIL) '((NIL NIL T) (NIL NIL _))))
; (print (reduction_variable_and_list '(T NIL NIL T) '((T _ _ T) (T T NIL T) (NIL T T T))))

;; есть ли элемент (elem) в списке (L)
(defun find_list (elem L)
	(cond ((null L) NIL)
		  ((equal elem (car L)))
		  (T (find_list elem (cdr L)))
	)
)

;; добавление элемента (elem) в список (L),
;; если этого элемента нет в списке
(defun unique_cons (elem L)
	(cond ((find_list elem L) L)
		  (T (cons elem L))
	)
)

;; объединение множеств L1 L2
(defun union_list (L1 L2)
	(reduce #'(lambda (elem new_L) (unique_cons elem new_L))
			L1
			:initial-value L2
			:from-end T
	)
)

; (print (union_list '((a) (b) (e)) '((a) (c) (d))))

;; добавление элемента (elem) в список new_L,
;; если (elem) есть в L
(defun add_to_newL_if_in_L (elem L new_L)
	(cond ((find_list elem L) (cons elem new_L))
		  (T new_L)
	)
)

; (print (add_to_newL_if_in_L '(b) '((b) (a)) '((d))))

;; пересечение множеств L1 L2
(defun intersection_list (L1 L2)
	(reduce #'(lambda (elem new_L) 
					  (add_to_newL_if_in_L elem L2 new_L)
			  )
			L1
			:initial-value ()
			:from-end T
	)
)

; (print (intersection_list '((a) (b) (e) (d)) '((a) (c) (e) (f) (r) (b))))

;; одна итерация сокращения (dnf),
;; представленную в виде списка значений переменных
;; *** ((0 1 1) (1 0 0) (1 0 1) (0 1 0)) -> ((0 1 _) (1 0 _))
(defun step_reduction (dnf)
	(reduce 'union_list 
			(mapcar #'(lambda (variable) (reduction_variable_and_list variable
																	  dnf
										 )
					  ) 
					  dnf
			)
			:initial-value ()
	)
)

; (print (step_reduction '((NIL NIL NIL) (NIL NIL T) (NIl T NIL) (NIL T T) (T NIL T))))
; (print (step_reduction '((0 1) (1 1) (1 1) (1 0))))

;; сокращает (DNF)
(defun reduction (dnf)
	(let ((next_dnf (step_reduction dnf))
		 )
		 (cond ((eql (length (intersection_list dnf next_dnf))
		 			 (length dnf)
		 		)
		 		next_dnf
			   )
		 	   (T (reduction next_dnf))
		 )
	)
)

; (print (reduction '((NIL NIL NIL) (NIL NIL T) (NIl T NIL) (NIL T T) (T NIL T))))
; (print (reduction '((NIL NIL) (NIL T) (T NIL) (T T))))

;; возвразает входной символ (symbol),
;; если она является допустимой переменной,
;; иначе NIL
(defun is_variable (symbol)
	(cond ((null symbol) NIL)
		  ((or (eql symbol 'v)
		  	   (eql symbol '&)
		  	   (eql symbol '-)
		  	   (eql symbol '->)
		  	   (eql symbol 'TRUE)
		  	   (eql symbol 'FALSE)
		   )
		   NIL
		  )
		  (symbol)
	)
)

;; кладет все переменные выражения (expr) в список (L)
(defun put_all_variables_to_L (expr L)
	(cond ((atom expr) (unique_cons (is_variable expr) L))
		  (T (reduce 'put_all_variables_to_L
		  			 expr
		  			 :initial-value L
		  			 :from-end T
		  	 )
		  )
	)
)

;; возврвщает список всех переменных,
;; входящих в выражение expr
(defun get_variables (expr)
	(delete_NIL (put_all_variables_to_L expr NIL))
)

; (print (get_variables '(X v - Y & (Z v X) & Y)))
; (print (put_all_variables_to_L '(X v - Y & (Z v X) & Y) '()))
; (print (put_all_variables_to_L '(or2 (and2 X Y)) '()))

;; соединяет элементы списка (L) символами (symbol)
(defun join (L symbol)
	(cond ((null (delete_NIL L)) (list 'T))
		  (T (cdr (reduce #'(lambda (elem new_L) (cons symbol (append elem new_L))
			  	   			)
				 			(delete_NIL L)
				 			:initial-value '()
				 			:from-end T
		 		  )
			 )
		  )
	) 	
)

;; печатает отдельную конъюнкцию
;; *** (X Y Z) , (T NIL T) -> X & - Y & Z
(defun form_conjuction (variables values)
	(join (mapcar #'(lambda (elem value) (cond ((eql value '_) NIL)
											   (value (list elem))
										 	   (T (cons '- (list elem)))
								   		 )
			  		)
				  variables
				  values
		  )
		  '&
	)
)

; (print (form_conjuction '(X Y Z) '(T NIL _)))

;; печатает входную (dnf) в читаемом виде
(defun form_dnf (variables dnf)
	(cond ((null dnf) (list 'NIL))
		  (T (join (mapcar #'(lambda (conjuction) (form_conjuction variables conjuction)) 
				  		   dnf
		  	 	   )
		  	 'v
		  	 )
		  )
	)
)

;; разделение выражения (expr) по символу (symbol)
;; *** '(a b c & d & e v r) '&   ->   ((a b c) (d) (e v r))
(defun split (expr symbol)
	(reduce #'(lambda (elem L) (cond ((eql elem symbol) (cons '() L))
									 (T (cons (cons elem (car L)) 
									 		  (cdr L)
									 	)
									 )
							   )
			  )
			expr
			:initial-value '(())
			:from-end T
	)
)

; (print (split '(a v c) '&))
; (print (split '(a b c & d & e v r) '&))

;; замена "A -> B" на "(impl A B)" в выражении (expr)
(defun change_impl (expr)
	(let* ((split_impl (split expr '->))
		  )
		  (cond ((null (cdr split_impl)) (change_dej (car split_impl)))
		  	    (T (reduce #'(lambda (L elem) (list (append '(impl)
		  	    											 ; (print "L: ")
		  	    											 ; (princ L)
		  	    											 L
		  	    											 ; (print "elem: ")
		  	    											 ; (princ elem)
		  	    											 (change_dej elem) 
		  	    									)
		  	   								  )
		  	    		     )
		  	    		   (cdr split_impl)
		  	    		   :initial-value (change_dej (car split_impl))
		  	       )
		  	    )
		  )
	)
)

;; замена "A v B" на "(or2 A B)" в выражении (expr)
(defun change_dej (expr)
	(let* ((split_dej (split expr 'v))
		  )
		  (cond ((null (cdr split_dej)) (change_conj (car split_dej)))
		  	    (T (reduce #'(lambda (L elem) (list (append '(or2)
		  	    											 L
		  	    											 (change_conj elem) 
		  	    									)
		  	   								  )
		  	    		     )
		  	    		   (cdr split_dej)
		  	    		   :initial-value (change_conj (car split_dej))
		  	       )
		  	    )
		  )
	)
)

;; замена "A & B" на "(and2 A B)" в выражении (expr)
(defun change_conj (expr)
	(let* ((split_conj (split expr '&))
		  )
		  (cond ((null (cdr split_conj)) (change_not (car split_conj)))
		  	    (T (reduce #'(lambda (L elem) (list (append '(and2)
		  	    											 L
		  	    											 (change_not elem) 
		  	    									)
		  	   								  )
		  	    		     )
		  	    		   (cdr split_conj)
		  	    		   :initial-value (change_not (car split_conj))
		  	       )
		  	    )
		  )
	)
)

;; замена "- A" на "(not A)" в выражении (expr)
(defun change_not (expr)
	(cond ((null expr) NIL)
		  ((atom expr) expr)
		  ((eql (car expr) '-) (list (cons 'not (change_impl (cdr expr)))))
		  ((atom (car expr)) (list expr))
		  (T (change_impl (car expr)))
	)
)

; (print (change_impl '(a -> (b v - c & d) v - e)))

;; преобразование простого выражения (expr) в
;; булевское представление lisp
;; *** (x & (y -> - z))   ->  (and x (impl y (not z))) 
(defun expr_to_FAL (expr variables)
	(let ((FAL (car (change_impl expr))))
		 (cond ((check_valid_expr FAL variables) FAL)
		  	   (NIL) 
		 )
	)
)

; (print (form_dnf '(X Y Z) '((NIL T T) (T T NIL))))

;; преобразование выражения (expr) в DNF
(defun FALtoDNF (expr)
	(let* ((variables (get_variables expr))
		   (FAL (expr_to_FAL expr variables))
		   (sdnf (FAL_to_SDNF variables FAL))
		   (dnf (reduction sdnf))
		  )
		  (print "SDNF: ")
		  (print (form_dnf variables sdnf))
		  (print "DNF: ")
		  (print (form_dnf variables dnf))
	)
)

;; печать преобразования FAL to DNF
(defun print_FALtoDNF (expr)
	(terpri)
	(princ "______________________________")
	(terpri)
	(princ "FAL: ")
	(princ expr)
	(terpri)
	; (princ "DNF: ")
	; (princ (FALtoDNF expr))
	(FALtoDNF expr)
	(terpri)
)

(print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
(print "Тест корректных выражений")
(terpri)
; (print_FALtoDNF '((- A v D v NIL) -> - (B & - C v D)))
; (print_FALtoDNF '(a -> (b v - c & d) v - e))
; (print_FALtoDNF '(- (A & T & - B) -> C -> - B))
; (print_FALtoDNF '(T & NIL))
; (print_FALtoDNF '(A))
; (print_FALtoDNF '((- A v D v NIL) -> - (B & - C v D)))
; (print_FALtoDNF '(A & - A -> B & - B -> C & - C))
; (print_FALtoDNF '(- (A v B & - C)))
; (print_FALtoDNF '(- (A & - B v B & - C v - D)))
; (print_FALtoDNF '(- (A & - B v B & - C)))
; (print_FALtoDNF '(- (A & B & - C v A & B & D v B & C & D)))
(print_FALtoDNF '(NIL -> NIL -> NIL))
; (print_FALtoDNF '(NIL -> NIL -> T))
; (print_FALtoDNF '(T & T))
; (print_FALtoDNF '(X v - Y))
; (print_FALtoDNF '(X v - X v Y v - Y))
; (print_FALtoDNF '(A & - A -> B v - B))


; (print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
; (print "Тест НЕкорректных выражений")
; (terpri)
; (print_FALtoDNF '(A v B & ))
; (print_FALtoDNF '(A v B & - -> D))
; (print_FALtoDNF '(A & v B))
; (print_FALtoDNF '(A B))

(terpri)