;;;; Created on 2014-05-14 15:22:31
(in-package :user)

; MACROS
; MACROS
; MACROS
(defmacro while (test &rest body)
  (list* 'loop
         (list 'unless test '(return nil))
         body))

;"melhor.abordagem" x
;"a*.melhor.heuristica" x
;"a*.melhor.heuristica.alternativa" x
;"sondagem.iterativa" x
;"ILDS" x
;"abordagem.alternativa" x

(defun resolve-sokoban(ficheiro-problema estrategia) 
  
  ;(setf (sb-ext:bytes-consed-between-gcs) (* 100 1024 1024)) ; set gc cycle to 100MB
  (if (not (validar-estrategia estrategia)) (throw 'estrategia-invalida "A estretegia requerida não é válida"))
  
  (let* (
         (tempo-inicio (get-internal-run-time))
         (conteudo-ficheiro-problema (parse-ficheiro ficheiro-problema))
         (mapa (nth 0 conteudo-ficheiro-problema))
         (caixas (nth 1 conteudo-ficheiro-problema) )
         (agente (nth 2 conteudo-ficheiro-problema))
         (operadores (concluir-operadores caixas)) 
         (estado (make-estado :mapa mapa :caixas caixas :agente agente) )
         )
    
    (let (
          (problema (cria-problema estado
                                   operadores
                                   :objectivo? #'estado-objectivo
                                   :heuristica (if (or (string= estrategia "abordagem.alternativa") (string= estrategia "a*.melhor.heuristica") (string= estrategia "melhor.abordagem") ) #'estado-heuristica1 #'estado-heuristica2)
                                   ))
          )
      
      (definir-mapa estado)
      (defparameter *deadlocks* (becos problema 3));(length caixas)
      (defparameter *nos-expandidos* 0)
      (defparameter *nos-gerados* 0)
      (defparameter *generated-states* (make-hash-table))
      ; )
      (convert2return (nth 0 (minha-procura problema 
                                            (cond 
                                              (
                                               (or (string= estrategia "melhor.abordagem") 
                                                   (string= estrategia "a*.melhor.heuristica") 
                                                   (string= estrategia "a*.melhor.heuristica.alternativa")
                                                   (string= estrategia "abordagem.alternativa")
                                                   ) "a*"
                                               )
                                              (t estrategia) ; "sondagem.iterativa" "ILDS"
                                              )
                                            tempo-inicio
                                            )
                           )
                      )
      )
    
    )
  
  )

; BECOS
; BECOS
; BECOS
(defun ha-caixa-em-canto (estado index-caixa)
  "Se existe uma caixa encostada a um canto formada por paredes ou outras caixas e a posição não é de destino. Parece que passar o index da caixa movimentada de um estado válido não introduz nenhuma melhoria antes pelo contrário."
  (labels 
    (
     
     (ha-volta (l c) (values (list (list (1+ l) c) (list (1+ l) (1+ c)) (list l (1+ c)) (list (1- l) (1+ c)) (list (1- l) c) (list (1- l) (1- c)) (list l (1- c)) (list (1+ l) (1- c)) (list (1+ l) c) (list (1+ l) (1+ c)))))
     (mesma-linha (segmento) 
       (let 
         ((l (nth 0 (nth 0 segmento)))) 
         (dolist (pos segmento) (if (not (= l (nth 0 pos))) (return-from mesma-linha nil))))
       (values t)
       )
     (mesma-coluna (segmento) 
       (let 
         ((c (nth 1 (nth 0 segmento)))) 
         (dolist (pos segmento) (if (not (= c (nth 1 pos))) (return-from mesma-coluna nil))))
       (values t))
     )
    
    ;(estado-caixas estado)
    (dolist (c (if index-caixa (list (nth index-caixa (estado-caixas estado))) (estado-caixas estado))
               
             )
      
      (let ((em-destino (find-in-list c (mapa-sokoban-destinos (estado-mapa estado))))) 
        (if (not em-destino)
            (let ((icanto nil)) 
              (let ((resultado
                     (loop for v in (ha-volta (nth 0 c) (nth 1 c)) do
                           (let* 
                             (
                              (box? (find-in-list v (estado-caixas estado))) 
                              (wall? (aref (mapa-sokoban-mapa (estado-mapa estado)) (nth 0 v) (nth 1 v)))
                              (obstacle? (or box? wall?))
                              )
                             
                             (if obstacle? 
                                 (setq icanto (append icanto (list v))) (setq icanto nil))
                             
                             (if (and 
                                  (>= (length icanto) 3) 
                                  (not (mesma-linha icanto)) 
                                  (not (mesma-coluna icanto))
                                  )
                                 (return t)
                                 )
                             )
                           );end loop
                     )) 
                (if resultado (return t))
                )
              )
            )
        )
      )
    ) 
  )


(defun eh-beco (estado lista-becos)
  (let* (
         (resultado nil)
         (agente (estado-agente estado))
         (caixas (estado-caixas estado))
         ) 
    
    (dolist (beco lista-becos)
      
      (if (= (length (nth 0 beco)) (length (intersection (nth 0 beco) caixas :test #'equal)))
          
          (let (
                (caixas-orig  (copy-2d-list (estado-caixas estado)))
                ) 
            
            
            (setf (estado-caixas estado) (nth 0 beco))
            
            (let* (
                   (areas (etiquetagem estado t))
                   (agente-area (aref areas (nth 0 agente) (nth 1 agente)))
                   )
              
              (if (= agente-area (nth 1 beco)) (setq resultado t))
              
              )
            (setf (estado-caixas estado) caixas-orig)
            
            (if resultado (return t))
            )
          
          )
      
      )
    )
  )

(defun becos (problema ncaixas)
  (let* (
         
         (inicio-time (get-universal-time))
         (timeout 300) ; One minute timeout
         
         (resultado nil)
         (estado-inicial (problema-estado-inicial problema))
         (estado (meu-copy-estado (problema-estado-inicial problema)))
         (combinacoes nil)
         (ncaixas-combo nil)
         )
    
    (setf (problema-estado-inicial problema) estado)
    
    ;dolist (ncaixas-combo combinacoes)
    (do ((c 1 (1+ c))) ((equal c (1+ ncaixas)))
      
      (setq combinacoes (avaliar-combinacoes estado combinacoes 1 c inicio-time timeout))
      (setq ncaixas-combo (nth (1- (length combinacoes)) combinacoes))
      
      (while (and 
              (not (equal ncaixas-combo nil))
              
              )
             
             (if (>= (- (get-universal-time) inicio-time) timeout) (return-from becos resultado))
             
             (let ((caixas (car ncaixas-combo)))
               
               (setq ncaixas-combo (cdr ncaixas-combo))
               
               (setf (estado-caixas estado) caixas)
               (setf (problema-operadores problema) (concluir-operadores caixas))
               
               (let* (
                      (etiq (etiquetagem estado t))
                      (areas (list-areas etiq))
                      )
                 
                 (dolist (area areas)
                   
                   (defparameter *nos-expandidos* 0)
                   (defparameter *nos-gerados* 0)
                   (defparameter *generated-states* (make-hash-table))
                   (defparameter *deadlocks* nil)
                   
                   (setf (estado-agente estado) (obter-posicao-area etiq area caixas))
                   
                   (if (and
                        (not (estado-objectivo estado)) 
                        (not (eh-beco estado resultado))
                        )
                       
                       (let ((solucao (nth 0 (minha-procura problema "a*" 0)))) 
                         
                         (if (not solucao)
                             (progn 
                               
                               (setq resultado (append resultado (list (list caixas area))))
                               )
                             
                             (progn
                               (dolist (caixas-caminho (caixas-no-caminho solucao))
                                 (setq ncaixas-combo (remove-if (lambda (x) (equal caixas-caminho x)) ncaixas-combo))
                                 )
                               )
                             
                             )
                         )
                       
                       )
                   
                   )
                 )
               
               )
             
             )
      
      )
    
    (setf (problema-estado-inicial problema) estado-inicial)
    (setf (problema-operadores problema) (concluir-operadores (estado-caixas estado-inicial)))
    (values resultado)
    )
  )

(defun caixas-no-caminho (lista-resultado)
  (let ((resultado nil)) 
    (dolist (e lista-resultado)
      (setq resultado (append resultado (list (estado-caixas e))))
      )
    (values resultado)
    )
  )

(defun avaliar-combinacoes (estado computadas begin-caixas end-caixas inicio-time timeout)
  (let (
        (resultado computadas)
        (ultimo (list nil))
        (etiquetagem (etiquetagem estado nil))
        )
    
    (do ((i begin-caixas (1+ i))) ((equal i (1+ end-caixas)))
      
      (if (> (- (get-universal-time) inicio-time) timeout) (return-from avaliar-combinacoes resultado))
      
      (setq ultimo (adiciona-conjunto estado etiquetagem ultimo))
      (setq ultimo ultimo)
      (setq resultado (append resultado (list  ultimo)))
      )
    (values resultado)
    )
  )







(defun adiciona-conjunto (estado etiquetagem elems)
  
  (let* (
         (resultado nil)
         (mapa (mapa-sokoban-mapa (estado-mapa estado)))
         (linhas-mapa (array-dimension mapa 0)) (colunas-mapa (array-dimension mapa 1))
         (agente-area (aref etiquetagem (nth 0 (estado-agente estado)) (nth 1 (estado-agente estado)) ))
         )
    
    (do ((e 0 (1+ e))) ((equal e (length elems)))
      
      (let ((elem (nth e elems)))
        
        (do ((l 0 (1+ l))) ((equal l linhas-mapa))
          (do ((c 0 (1+ c))) ((equal c colunas-mapa))
            
            (if (and 
                 (not (equal (aref etiquetagem l c) t)) 
                 (= (aref etiquetagem l c) agente-area)
                 )
                (progn (setq resultado (append resultado (list (append elem (list (list l c)))))))
                )
            
            )
          )
        
        )
      
      )
    
    (values (remove-duplicates resultado :test (lambda(e1 e2) (= (length e1) (length (intersection e1 e2 :test #'equal))))))
    
    )
  
  )

(defun definir-mapa (estado)
  "Coloca o valor de T nos espaços que não pertencem ao mapa útil."
  (let* (
         (areas (etiquetagem estado nil)) 
         (area-agente (aref areas (nth 0 (estado-agente estado)) (nth 1 (estado-agente estado))))
         )
    
    (do ((l 0 (1+ l))) ((equal l (array-dimension areas 0)))
      (do ((c 0 (1+ c))) ((equal c (array-dimension areas 1)))
        (if (and 
             (not (eq (aref areas l c) t)) 
             (not (= area-agente (aref areas l c)))
             )
            (progn (setf (aref (mapa-sokoban-mapa (estado-mapa estado)) l c) t) )
            )
        )
      )
    )
  )

; AUXILARES
; AUXILARES
; AUXILARES

(defun concluir-operadores (caixas)
  (let (
        (operadores nil) 
        (nr-caixas (length caixas))
        )
    (do ((i 0 (1+ i))) ((equal i nr-caixas))
      (setq operadores (append operadores (list (list i (construir-operador i 1 0)) )))
      (setq operadores (append operadores (list (list i (construir-operador i -1 0)) )))
      (setq operadores (append operadores (list (list i (construir-operador i 0 1)) )))
      (setq operadores (append operadores (list (list i (construir-operador i 0 -1)) ))) 
      )
    (values operadores)
    )
  )

(defun etiquetagem (estado caixas-obstaculos?)
  (let ((etiquetagem (copy-array (mapa-sokoban-mapa (estado-mapa estado)))) (etiqueta 1)) 
    (if caixas-obstaculos? 
        (dolist (c (estado-caixas estado)) (setf (aref etiquetagem (nth 0 c) (nth 1 c)) t) )
        )
    (do ((l 0 (1+ l))) ((equal l (array-dimension etiquetagem 0)))
      (do ((c 0 (1+ c))) ((equal c (array-dimension etiquetagem 1)))
        (let ((been-label? (etiquetagem-area etiquetagem l c etiqueta)))
          (if been-label? (incf etiqueta))
          )
        )
      )
    (values etiquetagem)
    )
  )

(defun etiquetagem-area (etiquetagem l c label)
  (let ((linhas (array-dimension etiquetagem 0)) (colunas (array-dimension etiquetagem 1)))
    (if (and 
             (not (or (= l -1) (= c -1) (= l linhas) (= c colunas)))  
             (not (aref etiquetagem l c))
             )
        (progn
          (setf (aref etiquetagem l c) label)
          (etiquetagem-area etiquetagem (1- l) c label)
          (etiquetagem-area etiquetagem l (1+ c) label)
          (etiquetagem-area etiquetagem (1+ l) c label)
          (etiquetagem-area etiquetagem l (1- c) label)
          (return-from etiquetagem-area t)
          )
        )
    )
  )

(defun list-areas (etiquetagem)
  (let ((resultado nil)) 
    (do ((l 0 (1+ l))) ((equal l (array-dimension etiquetagem 0)))
      (do ((c 0 (1+ c))) ((equal c (array-dimension etiquetagem 1)))
        (let ((val (aref etiquetagem l c))) 
          (if (and
               (not (eq val t)) 
               (not (find val resultado))
               )
              (progn (setq resultado (append resultado (list val))))
              )
          )
        )
      )
    
    (values resultado)
    )
  )

(defun obter-posicao-area (etiq area caixas)
  (let ((resultado nil)) 
    (do ((l 0 (1+ l))) ((or resultado (equal l (array-dimension etiq 0))))
      (do ((c 0 (1+ c))) ((or resultado (equal c (array-dimension etiq 1))))
        
        (if (and 
             (not (eq (aref etiq l c) t))
             (= (aref etiq l c) area)
             ) 
            (setq resultado (list l c))
            )
        )
      )
    (values resultado)
    )
  )

;Taken from http://stackoverflow.com/questions/9444885/common-lisp-how-to-return-a-list-without-the-nth-element-of-a-given-list
(defun remove-nth (n l)
  (if (or (zerop n) (null l))
    (cdr l)
    (cons (car l) (remove-nth (1- n) (cdr l)))))

(defun find-in-list (f  l)
  (find-if #'(lambda(x) (and (equal (nth 0 x) (nth 0 f)) (equal (nth 1 x) (nth 1 f)))) l)
  )

(defun casa-livre (estado i j)
  (let ((mapa (mapa-sokoban-mapa (estado-mapa estado))) (mapa-aux (mapa-sokoban-mapa-aux (estado-mapa estado))))
    (if 
        (and 
         (< i (array-dimension mapa 0)) 
         (< j (array-dimension mapa 1))
         (>= i 0)
         (>= j 0)
         )
        (and (not (aref mapa i j)) (not (find-in-list (list i j) (estado-caixas estado))))
        (values nil)
        )
    )
  )

; CONVERT OUTPUT
; CONVERT OUTPUT
; CONVERT OUTPUT
(defun convert2return(input)
  
  (let ((final-result nil)) 
    (do ((i 0 (1+ i))) ((>= i (1- (length input))))
      (let* (
             (actual (nth i input)) 
             (proximo (nth (1+ i) input))
             (cmovida (caixa-movida actual proximo))
             )
        (setq final-result (append final-result (cdr (encontra-caminho 
                                                (estado-mapa actual)
                                                (estado-caixas actual) 
                                                (nth 0 (estado-agente actual)) (nth 1 (estado-agente actual)) 
                                                (- (nth 0 (nth (nth 0 cmovida) (estado-caixas actual))) (nth 0 (nth 1 cmovida))) 
                                                (- (nth 1 (nth (nth 0 cmovida) (estado-caixas actual))) (nth 1 (nth 1 cmovida))) 
                                                ))))
        (setq final-result (append final-result (list (nth (nth 0 cmovida) (estado-caixas actual)))))
        )
      )
    
    (if 
        (not (= (length input) 0))
        (setq final-result (append (list (estado-agente (nth 0 input))) final-result))
        )
    
    (values final-result) 
    )
  )

(defun caixa-movida(estadoI estadoF)
  (let* (
         (diff (set-difference (estado-caixas estadoI) (estado-caixas estadoF) :test #'equal))
         (index (search (list (nth 0 diff)) (estado-caixas estadoI) :test 'equal))
         (iniPos (nth index (estado-caixas estadoI)))
         (finPos (nth index (estado-caixas estadoF)))
         )
    (list
     index
     (list (- (nth 0 finPos) (nth 0 iniPos)) (- (nth 1 finPos) (nth 1 iniPos)) )
     )
    )
  )

; OBJECTIVO
; OBJECTIVO
; OBJECTIVO

(defun estado-objectivo (estado)
  (= (length (estado-caixas estado)) (length (intersection (estado-caixas estado) (mapa-sokoban-destinos (estado-mapa estado)) :test #'equal)))
  )

; OPERADORES
; OPERADORES
; OPERADORES

(defun construir-operador(index x y)
  #'(lambda (estado)
      
      (let ((index-caixa (nth index (estado-caixas estado))))
        (let (
              (x-i (+ (nth 0 index-caixa) x)) 
              (y-i (+ (nth 1 index-caixa) y)) 
              (x-f (- (nth 0 index-caixa) x)) 
              (y-f (- (nth 1 index-caixa) y)) 
              )
          
          (if (ha-caminho 
               (estado-mapa estado) 
               (estado-caixas estado) 
               (nth 0 (estado-agente estado)) 
               (nth 1 (estado-agente estado)) 
               x-i y-i)
              (if (casa-livre estado x-f y-f) 
                  (let ((novo-estado (meu-copy-estado estado)))
                    
                    (let ((caixa-index-antigo (nth index (estado-caixas novo-estado))))
                      (setf (nth 0 (estado-agente novo-estado)) (nth 0 caixa-index-antigo))
                      (setf (nth 1 (estado-agente novo-estado)) (nth 1 caixa-index-antigo))
                      )
                    (setf (nth 0 (nth index (estado-caixas novo-estado))) x-f)
                    (setf (nth 1 (nth index (estado-caixas novo-estado))) y-f)
                    (values (list novo-estado))
                    )
                  )
              )
          )
        )
      
      )
  )

; ESTADO
; ESTADO
; ESTADO

(defun meu-copy-estado(estado)
  (make-estado 
   :mapa (estado-mapa estado) 
   :caixas (copy-2d-list (estado-caixas estado)) 
   :agente (copy-list (estado-agente estado)))
  )

(defun copy-array (array-to-copy)
  (let ((new-array (make-array (array-dimensions array-to-copy))))
    (loop for i below (array-dimension array-to-copy 0) do
          (loop for j below (array-dimension array-to-copy 1) do
                (let ((v (aref array-to-copy i j)))
                  (setf (aref new-array i j) v)
                  )
                )
          )
    (values new-array)
    )
  )

; ESTADO
; ESTADO
; ESTADO

(defstruct estado 
  (mapa nil :type mapa-sokoban)
  (caixas nil :type list)
  (agente nil :type list))

(defun copy-2d-list (list-2-copy)
  (let ((new-list nil)) 
    (dolist (c list-2-copy) 
      (let ((line nil)) 
        (dolist (l c)
          (setq line (append line (list l)))
          )
        (setq new-list (append new-list (list line)))
        )
      )
    (values new-list)
    )
  )

; HEURISTICA
; HEURISTICA
; HEURISTICA

(defun estado-heuristica1 (estado)
  
  (let 
    (
     (nr-caixas  (length (estado-caixas estado))) 
     (nr-destinos (length (mapa-sokoban-destinos (estado-mapa estado))))
     (distancia-minima 0)
     )
    
    (do ((i 0 (1+ i))) ((equal i nr-destinos))
      (let (
            (distancia-minima-caixa nil)
            ) 
        (loop for c from 0 to (1- (length (estado-caixas estado))) do
              (let ((distancia-caixa (manhatten (nth c (estado-caixas estado)) (nth i (mapa-sokoban-destinos (estado-mapa estado)))))) 
                (if (or (not distancia-minima-caixa) (< distancia-caixa distancia-minima-caixa)) 
                    (setq distancia-minima-caixa distancia-caixa)
                    )
                )
              )
        (setq distancia-minima (+ distancia-minima distancia-minima-caixa))
        )
      )
    
    (values distancia-minima)
    
    )
  
  )

(defun estado-heuristica2 (estado)
  
  (let 
    (
     (caixas (loop for i from 0 to (1- (length (estado-caixas estado))) collect i))
     (nr-caixas  (length (estado-caixas estado))) 
     (nr-destinos (length (mapa-sokoban-destinos (estado-mapa estado))))
     (distancia-minima 0)
     )
    (do ((i 0 (1+ i))) ((equal i nr-destinos))
      (let (
            (distancia-minima-caixa nil)
            (sel-caixa nil)
            ) 
        (dolist (c caixas)
          (let ((distancia-caixa (manhatten (nth c (estado-caixas estado)) (nth i (mapa-sokoban-destinos (estado-mapa estado)))))) 
            (if (or (not distancia-minima-caixa) (< distancia-caixa distancia-minima-caixa)) 
                (progn 
                  (setq distancia-minima-caixa distancia-caixa)
                  (setq sel-caixa c)
                  )
                )
            )
          )
        
        (setq distancia-minima (+ distancia-minima distancia-minima-caixa))
        
        (remove-nth sel-caixa caixas)
        )
      )
    (values distancia-minima)
    
    )
  
  )

(defun manhatten(f s)
  (+ (abs (- (nth 0 f) (nth 0 s))) (abs (- (nth 1 f) (nth 1 s))))
  )

; VALIDATION
; VALIDATION
; VALIDATION

(defun validar-estrategia (estrategia)
  (setq estrategias-disponiveis (list "melhor.abordagem" "a*.melhor.heuristica" "a*.melhor.heuristica.alternativa" "sondagem.iterativa" "ILDS" "abordagem.alternativa"))
  (loop for i below (length estrategias-disponiveis) do
        (when (string= (nth i estrategias-disponiveis) estrategia) (return estrategia)) 
        )
  )















































;FICHEIRO PROCURA REDEFINIÇÂO
;FICHEIRO PROCURA REDEFINIÇÂO
;FICHEIRO PROCURA REDEFINIÇÂO
(defun minha-problema-gera-sucessores (problema estado)
  
  (let ((sucessores nil))
    (dolist (operador (problema-operadores problema))
      (let ((resultado (funcall (nth 1 operador) estado)))
        
        (if resultado
            
            (progn
              (setq resultado (nth 0 resultado)) 
              (let* (
                     (caixa-canto? (ha-caixa-em-canto resultado nil))
                     (caixa-beco? (eh-beco estado *deadlocks*))
                     (hash-code (hashcodeestado resultado)) 
                     (repetido? (gethash hash-code *generated-states*))
                     )
                
                (if (and 
                     (not repetido?)
                     (not caixa-beco?)
                     (not caixa-canto?)
                     )
                    (progn
                      (setf (gethash hash-code *generated-states*) t );resultado
                      (setf sucessores (nconc (list resultado) sucessores))
                      )
                    )
                
                )
              )
            
            ); end if
        
        )
      
      )
    
    (incf *nos-expandidos*)
    (incf *nos-gerados* (length sucessores))
    
    (values sucessores)
    )
  
  )
(defun hashcodeestado (estado)
  (let* 
    (
     (areas (etiquetagem estado t))
     (area-agente (aref areas (nth 0 (estado-agente estado)) (nth 1 (estado-agente estado))))
     (hash-code (hashcode (append (estado-caixas estado) (list areas) (list area-agente) )))
     )
    (values hash-code)
    )
  )
(defun hashcode (l)
  (let (
        (result 17) 
        (fl (flatten l))
        )
    (dolist (e fl)
      (if (arrayp e) 
          (dotimes (l (array-dimension e 0)) 
            (dotimes (c (array-dimension e 1))
              (setq result (+ (* 31 result) (sxhash (aref e l c))))
              )
            ) 
          (setq result (+ (* 31 result) e))
          )
      )
    (values result)
    )
  )

;Taken from http://stackoverflow.com/questions/2680864/how-to-remove-nested-parentheses-in-lisp
(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

(defun minha-espaco-expande-no (espaco no)
  "Expande o no recebido no espaco, actualizando a estrutura do
  espaco."
  ;; Comecamos por gerar todos os sucessores do estado correspondente
  ;; ao no recebido
  
  (let ((sucessores (minha-problema-gera-sucessores (espaco-problema espaco)
                                              (no-estado no))))
    ;; O no ja foi expandido, por isso passa para os expandidos
    (junta-no-expandido espaco no)
    
    ;(if sucessores (incf *profundidade-maxima*) (decf *profundidade-maxima*))
    
    ;; Finalmente, juntamos aos abertos os nos cujos estados ainda nao
    ;; existem no espaco (os nos mais recentes vao para o fim da
    ;; lista)
    (junta-nos-gerados espaco
                       (cria-nos-sucessores espaco no sucessores))))

(defun minha-procura-com-espaco (problema espaco)
  
  (let ((objectivo? (problema-objectivo? problema)))
    (loop
     
     ;; Quando nao temos mais nos e porque ja exploramos todo o
     ;; espaco e nao encontramos a solucao (nao existe)
     (when (espaco-vazio? espaco)
       (return nil))
     
     ;; Vamos considerar o no gerado mais antigo para termos uma
     ;; procura em largura primeiro
     (let ((proximo-no (espaco-proximo-no espaco))) 
       
       ;; Se atingimos a solucao paramos e devolvemos os estados no
       ;; caminho 
       (when (funcall objectivo? (no-estado proximo-no))
         (return (da-caminho proximo-no)))
       
       ;; Caso contrario, devemos expandir o no
       (minha-espaco-expande-no espaco proximo-no)))
    )
  )

(defun minha-profundidade-iterativa (problema profundidade-maxima)
  "Algoritmo de procura em profundidade iterativa."
  (block profundidade-iterativa
    (dotimes (prof (1+ profundidade-maxima))
      (let ((solucao (minha-profundidade-primeiro problema prof)))
	(when solucao 
	  (return-from profundidade-iterativa solucao))))))

(defun minha-a* (problema &key espaco-em-arvore?)
  
  (let ((espaco (novo-espaco-a* problema :espaco-em-arvore? espaco-em-arvore?)))
    (junta-nos-gerados espaco
                       (list (cria-no-a* (problema-estado-inicial problema)
                                         nil  ; O pai do estado inicial nao existe
                                         0    ; e o custo e' 0 (zero)
                                         (problema-heuristica problema))))
    
    (minha-procura-com-espaco problema espaco)))

(defun minha-ida* (problema &key espaco-em-arvore?)
  (let ((estado= (problema-estado= problema))
        (heur (problema-heuristica problema))
        (fun-custo (problema-custo problema))
        (objectivo? (problema-objectivo? problema)))
    
    (labels ((esta-no-caminho? (estado caminho)
               (unless espaco-em-arvore?
                 (member estado caminho :test estado=)))
             
             (prof (estado custo-max custo-caminho caminho)
               (block prof
                 (if (esta-no-caminho? estado caminho)
                     nil
                     (let ((custo (+ custo-caminho (funcall heur estado))))
                       (cond ((> custo custo-max) custo)
                         ((funcall objectivo? estado) (list estado))
                         (t
                          (let ((min-custo most-positive-fixnum))
                            (dolist (suc (minha-problema-gera-sucessores
                                          problema estado))
                              (let ((solucao (prof suc 
                                                   custo-max 
                                                   (+ custo-caminho
                                                      (funcall fun-custo suc))
                                                   (or espaco-em-arvore?
                                                       (cons estado
                                                             caminho)))))
                                (if (numberp solucao)
                                    (setf min-custo (min min-custo
                                                         solucao))
                                    (if solucao
                                        (return-from prof (cons estado
                                                                solucao))))))
                            min-custo))))))))
      
      (let ((custo-max 0))
        (loop
         (let ((solucao (prof (problema-estado-inicial problema)
                              custo-max
                              0
                              nil)))
           (if (numberp solucao)
               (if (> solucao custo-max)
                   (setf custo-max solucao)
                   (return nil))
               (return solucao))))))))

(defun minha-profundidade-primeiro (problema profundidade-maxima) 
  "Algoritmo de procura em profundidade primeiro."

  (let ((estado= (problema-estado= problema))
	(objectivo? (problema-objectivo? problema)))

    (labels ((esta-no-caminho? (estado caminho)
	       (member estado caminho :test estado=))
	     
	     (procura-prof (estado caminho prof-actual)
	       (block procura-prof
		 
		 ;; base da recursao:
		 ;; 1. quando comecamos a repetir estados pelos quais ja
		 ;;    passamos no caminho que esta a ser percorrido
		 ;;    (para evitar caminhos infinitos)
		 ;; 2. quando atingimos o objectivo
		 ;; 3. quando ultrapassamos a profundidade limite ate
		 ;;    onde se deve efectuar a procura
		 (cond ((funcall objectivo? estado) (list estado))
		       ((= prof-actual profundidade-maxima) nil)
		       ((esta-no-caminho? estado caminho) nil)
		       (t 
			(dolist (suc (minha-problema-gera-sucessores problema
							       estado))
			  ;; avancamos recursivamente, em profundidade,
			  ;; para cada sucessor
			  (let ((solucao (procura-prof suc (cons estado caminho) (1+ prof-actual))))
					(when solucao
						(return-from procura-prof (cons estado solucao))
					)
				)))))))
      
      (procura-prof (problema-estado-inicial problema) nil 0))))

(defun minha-procura (problema tipo-procura tempo-inicio
                               &key (profundidade-maxima most-positive-fixnum)
                               (espaco-em-arvore? nil))
  "Dado um problema e um tipo de procura devolve uma lista com: a
  solucao para o problema (a lista de estados desde o estado inicial
  ate' ao estado final), ou nil caso nao encontre a solucao; tempo
  gasto na procura (em internal-time-units); numero de nos expandidos;
  numero de nos gerados."
  
  (flet ((faz-a-procura (problema tipo-procura 
                                  profundidade-maxima espaco-em-arvore?)
           ;; Usamos cond em vez de case porque nao sabemos de que
           ;; package veem os simbolos (o string-equal funciona com o
           ;; symbol-name do simbolo e e' "case-insensitive")
           
           ;; Actualmente, apenas a procura em largura, o A* e o IDA*
           ;; estao a aproveitar a informacao do espaco de estados ser
           ;; uma arvore
           
           (cond ((string-equal tipo-procura "largura")
                  (largura-primeiro problema 
                                    :espaco-em-arvore? espaco-em-arvore?))
             ((string-equal tipo-procura "profundidade")
              (minha-profundidade-primeiro problema profundidade-maxima))
             ((string-equal tipo-procura "profundidade-iterativa")
              (minha-profundidade-iterativa problema profundidade-maxima))
             ((string-equal tipo-procura "a*")
              (minha-a* problema :espaco-em-arvore? espaco-em-arvore?))
             ((string-equal tipo-procura "ida*")
              (minha-ida* problema :espaco-em-arvore? espaco-em-arvore?))
             
             ((string-equal tipo-procura "ilds")
              (ilds problema 300 0 250))
             ;((string-equal tipo-procura "dds"))
             ((string-equal tipo-procura "sondagem.iterativa")
              (iterative-sampling problema 250 100000)
              )
             )
           )
         )
    
    (let ((*nos-gerados* 0)
          (*nos-expandidos* 0))
      (let ((solucao (faz-a-procura problema tipo-procura
                                    profundidade-maxima
                                    espaco-em-arvore?)))
        
        ;(print *NOS-EXPANDIDOS*)
        ;(print *NOS-GERADOS*) 
        
        (list solucao (- (get-internal-run-time) tempo-inicio) *nos-expandidos* *nos-gerados*)))
    
    )
  )














































; ITERATIVE-SAMPLING
; ITERATIVE-SAMPLING
; ITERATIVE-SAMPLING
(defun iterative-sampling (problema profundidade-maxima iteracoes)
  
  (let ((solucao nil)) 
    (do ((i 0 (1+ i))) ((or solucao (equal i iteracoes)))
      (setq solucao (one-samp problema profundidade-maxima))
      )
    (values solucao)
    )
  
  )

; ONE-SAMP
; ONE-SAMP
; ONE-SAMP
(defun one-samp (problema profundidade-maxima)
  "Algoritmo de procura onde sample search."
  
  (defvar *NOS-EXPANDIDOS* 0)
  (defvar *NOS-GERADOS* 0)
  
  (labels 
    
    ((esta-no-caminho? (estado caminho) (member estado caminho :test (problema-estado= problema)))
     
     (one-samp-step (estado caminho prof-actual)
       
       (cond
         ((funcall (problema-objectivo? problema) estado) (list estado))
         ((= prof-actual profundidade-maxima) nil)
         ((esta-no-caminho? estado caminho) nil)
         (t 
          
          (let ((suc (minha-problema-gera-sucessores problema estado))) 
            (if suc 
                (let* ( 
                       
                       (selprob (nth (random (length suc)) suc))
                       (solucao (one-samp-step selprob (cons estado caminho) (1+ prof-actual)))
                       )
                  (when solucao (return-from one-samp-step (cons estado solucao)))
                  )
                (values nil)
                )
            )
          
          )
         )
       )
     )
      (one-samp-step (problema-estado-inicial problema) nil 0)
    )
  
  )

;ILDS
;ILDS
;ILDS
(defun ilds(problema profundidade-maxima discrepancia-inicial discrepancia-final)
  (let ((solucao nil)) 
    (do ((i discrepancia-inicial (1+ i))) ((or solucao (equal i (1+ discrepancia-final))))
      (setq solucao (ilds-step problema profundidade-maxima i))
      )
    (values solucao)
    )
  )
(defun ilds-step (problema profundidade-maxima discrepancia)
  "Algoritmo de procura iterative limited discrepancy search step. O ilds pode sofrer de trasing já que dá prioridade à profundidade."
  
  (defvar *NOS-EXPANDIDOS* 0)
  (defvar *NOS-GERADOS* 0)
  
  (labels 
    
    ((esta-no-caminho? (estado caminho) (member estado caminho :test (problema-estado= problema)))
     
     (ilds-step-step (estado caminho prof-actual discrepancias-restantes)
       
       (cond
         ((funcall (problema-objectivo? problema) estado) (list estado))
         ((= prof-actual profundidade-maxima) nil)
         ((esta-no-caminho? estado caminho) nil)
         (t 
          (let ((suc (minha-problema-gera-sucessores problema estado)))
            (if suc
                (progn 
                  (setq suc (sort suc #'< :key (lambda (v) (funcall (problema-heuristica problema) v) )))
                  ;(print "Step")
                  ;(print prof-actual)
                  ;(print discrepancias-restantes)
                  (if (> discrepancias-restantes 0) 
                      (do ((i (1- (length suc)) (1- i))) ((equal i 0)) 
                        (progn 
                          (let ((solucao-worst (ilds-step-step (nth i suc) (cons estado caminho) (1+ prof-actual) (1- discrepancias-restantes))))
                          	(when solucao-worst (return-from ilds-step-step (cons estado solucao-worst)))
                          )
                        )
                      )
                  )
                (progn (let ((solucao-best (ilds-step-step (nth 0 suc) (cons estado caminho) (1+ prof-actual) discrepancias-restantes)))
                         (when solucao-best (return-from ilds-step-step (cons estado solucao-best)))
                         ))
                )
            (values nil)
            )
          )
         )
       )
     )
    
    )
      (ilds-step-step (problema-estado-inicial problema) nil 0 discrepancia)
    )
  )
  