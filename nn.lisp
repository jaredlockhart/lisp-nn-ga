(defun network-evolve (&key (layers '(2 3 1))(popcap 100)(fitness #'xor-trial)(generations 100)(report 10)(trials 200))
  (let ((population (population-make layers popcap)))
    (progn (dotimes (generation generations)
             (progn 
               (let ((futurepop (population-generation population fitness trials)))
                      (setf population futurepop))
               (if (eql (mod generation report) 0)
                   (format t "~%Generation ~A Top Fitness ~A Avg Fitness ~A" 
                           generation
                           (population-top-fitness population)
                           (population-avg-fitness population)))))
      (saveas "~%Evolution complete.  Save population as: " population))))



(defun sigmoid (x)
  (/ (1+ (exp (- x)))))

(defmacro dopopulation ((pool layer population &optional (return)) body)
  `(dovector (,layer ,population ,return)
      (dovector (,pool ,layer)
          ,body)))

(defmacro donetwork ((neuron layer network &optional (return)) body)
  `(dovector (,layer ,network ,return)
       (dovector (,neuron ,layer)
           ,body)))

(defmacro collect-network ((neuron layer network) body)
  `(vcollect-vector (,layer ,network)
     (vcollect-vector (,neuron ,layer)
        ,body)))      

(defmacro collect-population ((pool layer population) body)
  `(vcollect-vector (,layer ,population)
      (vcollect-vector (,pool ,layer)
           ,body)))

(defun population-make (layers popcap)
  (let ((population #()))
    (while ((second layers) population)
           (let ((inputs (first layers))
                 (neurons (second layers)))
             (setf population
                   (vappend
                    (layer-make popcap neurons inputs) population))
             (setf layers (rest layers))))))

(defun layer-make (popcap neurons inputs)
  (vcollect-times (neuron neurons)
                  (pool-make popcap inputs)))

(defun pool-make (popcap inputs)
  (vcollect-times (neuron popcap)
                  (neuron-make inputs)))
(defun neuron-make (inputs)
  (make-neuron :weights
               (vcollect-times (n inputs) (+- (random 1.0)))))



(defstruct neuron
  (name (vcollect-times (n 8) (random 10)))
  (friends #())
  (weights #())
  (trials 0)
  (fitness 0)
  (improvement 0))


(defun network-feedforward (network pattern)
  (if network
      (let* ((layer (vfirst network))
             (activations (vcollect-vector (neuron layer)
                             (mapvect #'* (neuron-weights neuron) pattern)))
             (output (vcollect-vector (neuron activations)
                             (sigmoid (vapply #'+ neuron)))))
        (network-feedforward (vrest network) output))
    pattern))

(defun population-generation (population fitness trials)
  (let* ((trialed (population-trial population fitness trials))
         (top-fitness (population-sort population #'better-man))
         (top-improved (population-sort population #'improved))
         (top-mutated (population-mutate (population-vupto 0.1 top-fitness)))
         (top-bred (population-crossover (population-vupto 0.1 top-fitness)))
         (all-mutated (population-mutate (population-gradient 0.5 trialed)))
         (improved-mutated (population-mutate (population-vupto 0.2 top-improved)))
         (random-new (population-clear (population-vupto 0.05 trialed)))
         (future (population-conjoin  top-mutated
                                      top-bred
                                      all-mutated
                                      improved-mutated
                                      random-new
                                      (population-vupto 0.05 top-fitness))))
    future))

(defun improved (neuron1 neuron2)
  (let* ((first-improve (neuron-improvement neuron1))
         (second-improve (neuron-improvement neuron2)))
    (if (> first-improve second-improve)
        T)))

(defun population-clear (population)
  (collect-population (pool layer population)
    (mapvect #'(lambda (neuron)
                 (neuron-make (length (neuron-weights neuron))))
             pool)))
  

(defun population-trial (population fitness trials)
  (progn
    (dotimes (trial (round (/ trials 2)))
      (let* ((network (network-make-friends population))
             (fitness (funcall fitness network)))
        (donetwork (neuron layer network)
                   (progn
                     (if (> fitness (neuron-fitness neuron))                             
                         (progn
                           (setf (neuron-improvement neuron) (- fitness (neuron-fitness neuron)))
                           (setf (neuron-fitness neuron) fitness)
                           (let* ((friends (collect-network (neuron layer network) (neuron-name neuron))))
                             (donetwork (neuron layer network)
                                        (setf (neuron-friends neuron) friends)))))
                     (incf (neuron-trials neuron))))))
    (dotimes (trial (round (/ trials 2)))
      (let* ((network (network-make-random population))
             (fitness (funcall fitness network)))
        (donetwork (neuron layer network)
                   (progn
                     (if (> fitness (neuron-fitness neuron))
                         (progn
                           (setf (neuron-improvement neuron) (- fitness (neuron-fitness neuron)))
                           (setf (neuron-fitness neuron) fitness)))
                     (incf (neuron-trials neuron))
                     (let* ((friends (collect-network (neuron layer network) (neuron-name neuron))))
                       (donetwork (neuron layer network)
                                  (setf (neuron-friends neuron) friends)))))))
    population))


(defun network-make-random (population)
  (collect-population (pool layer population)
     (aref pool (random (length pool)))))              

(defun network-make-friends (population)
  (let ((neuron (vrandom-in (vrandom-in (vrandom-in population)))))
    (if (> (length (neuron-friends neuron)) 0)
        (let ((friends (neuron-friends neuron)))
          (vcollect-vector (layer population :index laycurr)
                           (vcollect-vector (neuron layer :index neurcurr)
                                            (neuron-by-name (aref (aref friends laycurr) neurcurr) neuron))))
      (network-make-random population))))

(defun neuron-by-name (name pool)
  (let ((looked-for (dovector (neuron pool)
                              (if (veql (neuron-name neuron) name)
                                  (return neuron)))))
    (if looked-for
        looked-for
      (aref pool (random (length pool))))))
             
(defun population-sort (population test)
  (collect-population (pool layer population)
    (sort pool #'(lambda (neuron1 neuron2)
                   (funcall test neuron1 neuron2)))))

(defun better-man (neuron1 neuron2)
  (let* ((first-fitness (neuron-fitness neuron1))
         (second-fitness (neuron-fitness neuron2)))
    (if (> first-fitness second-fitness)
        T)))
        

(defun population-vupto (percent population)
  (collect-population (pool layer population)
    (vupto (round (* percent (length pool))) pool)))



(defun population-gradient (percent population)
  (collect-population (pool layer population)
    (gradient (round (* percent (length pool))) pool)))

(defun gradient (size pool)
  (let ((collected #()))
    (while ((< (length collected) size) collected)
      (let* ((first-neuron (aref pool (random (length pool))))
             (second-neuron (aref pool (random (length pool))))
             (winner (if (better-man first-neuron second-neuron)
                         first-neuron
                       second-neuron)))
       (if (not (vmember winner collected))
           (setf collected (vcons winner collected)))))))


(defun population-mutate (population)
  (collect-population (pool layer population)
    (vcollect-vector (neuron pool)
      (let* ((name (neuron-name neuron))
             (friends (neuron-friends neuron))
             (weights (mapvect #'(lambda (weight) (* weight (+ 1.0 (+- (random 0.2))))) (neuron-weights neuron))))
        (make-neuron :weights weights
                     :name name
                     :friends friends)))))
                     
(defun population-crossover (population)
  (collect-population (pool layer population)
    (vflatten (vcollect-vector (neuron (vupto (round (/ (length pool) 2)) (vrest pool)))
      (let* ((n (random (length (neuron-weights neuron))))
             (father neuron)
             (mother (pool-find-mate pool father))
             (baby1 (make-neuron :weights (vconjoin (vupto n (neuron-weights father)) (vafter n (neuron-weights mother)))))
             (baby2 (make-neuron :weights (vconjoin (vupto n (neuron-weights mother)) (vafter n (neuron-weights father))))))
        (vector baby1 baby2))))))
             
(defun pool-find-mate (pool neuron)
  (dovector (testneuron pool)
    (if (>= (neuron-fitness testneuron) (neuron-fitness neuron))
        (return testneuron))))

(defun population-conjoin (&rest populations)
  (vcollect-times (layer (length (first populations)))
     (vcollect-times (pool (length (aref (first populations) (1- layer))))
        (apply #'vconjoin (mapcar #'(lambda (population)
                                      (aref (aref population (1- layer)) (1- pool)))
                                  populations)))))

(defun xor-trial (network)
  (let ((fitness 0))
    (dovector (x #(0 1) fitness)
      (dovector (y #(0 1))
         (let* ((xor (if (eql x y) 0 1))
                (output (vfirst (network-feedforward network (vector x y)))))
           (incf fitness
                 (- 1 (abs (- xor output)))))))))

(defun population-avg-fitness (population)
  (let ((allfitness 0)
        (neurons 0))
    (dopopulation (pool layer population (float (/ allfitness neurons)))
      (dovector (neuron pool)
        (progn
          (incf allfitness (neuron-fitness neuron))
          (incf neurons))))))

(defun population-top-fitness (population)
  (let ((fitness 0))
    (dopopulation (pool layer population fitness)
      (dovector (neuron pool)
        (if (> (neuron-fitness neuron) fitness)
            (setf fitness (neuron-fitness neuron)))))))

(defun saveas (string value)
  (progn
    (format t string)
    (let ((name (read)))
      (eval `(defvar ,name ,value)))))


(defun average (&rest rest)
  "Given a list of numbers, returns their average"
  (float (/ (apply #'+ rest) (length rest))))

(defun vfirst (vector)
  "Returns the first element of a vector"
  (aref vector 0))

(defun vlast (vector)
  "Last element of a vector"
  (aref vector (1- (length vector))))

(defun vapply (fn vector)
  "Apply for vectors"
  (apply fn (coerce vector 'list)))

(defun vrandom (vector)
  (aref vector (random (length vector))))

(defun vcons (atom vector)
  "Cons for vectors"
  (let ((newvector (make-array (1+ (length vector)))))
    (setf (aref newvector 0) atom)
    (loop for x from 1 to (length vector) do
          (setf (aref newvector x) (aref vector (1- x))))
    newvector))

(defun vflatten (x)
  "Given a vector of embedded vectors, creates one layer of vector"
  (labels ((rec (x acc)
             (cond ((vnull x) acc)
                   ((not (vectorp x)) (vcons x acc))
                   (t (rec (vfirst x) (rec (vrest x) acc))))))
    (rec x #()))) 

(defun vgroup (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (vafter n source)))
               (if rest
                   (rec rest (vcons (subseq source 0 n) acc))
                   (nreverse (vcons source acc))))))
    (if source (rec source #()) #())))

(defun vnull (vector)
  "Null for vectors"
  (if (vectorp vector) (if (eql (length vector) 0) T) (null vector)))

(defun vsecond (vector)
  "Returns the second element of a vector"
  (aref vector 1))

(defun vrest (vector)
  "Returns everything after the first element of a vector"
  (if (> (length vector) 1)
      (subseq vector 1 (length vector))))

(defun vupto (index vector)
  "Returns all the elements of a vector up to INDEX"
  (if (< index (length vector))
      (subseq vector 0 index)
    vector))

(defun vafter (index vector)
  "Returns everything after the Nth element of a vector"
  (if (> index (1- (length vector)))
      nil
    (subseq vector index (length vector))))

(defun vappend (new vector)
  "Given two vectors it conjoins them, or an atom it appends it to the vector"
  (let ((newvector (make-array (1+ (length vector)))))
    (progn (replace newvector vector)
      (setf (aref newvector (length vector)) new)
      newvector)))

(defun vconjoin (&rest rest)
  (if (third rest)
      (apply #'vconjoin (cons (vconjoin (first rest) (second rest)) (nthcdr 2 rest)))
    (if (> (length (second rest)) 0)
        (vconjoin (vappend (vfirst (second rest)) (first rest)) (vrest (second rest)))
      (first rest))))

(defun mapvect (function vector &optional (vector2) (collected #()))
  "Maps a function across a vector"
  (if (> (length vector) 0)
      (if vector2
          (mapvect function (vrest vector) (vrest vector2)
                   (vappend (funcall function (vfirst vector) (vfirst vector2)) collected))
        (mapvect function (vrest vector) nil 
                 (vappend (funcall function (vfirst vector)) collected)))
    collected))
  
(defmacro vcollect-vector ((item vector &key (index (gentemp))) term)
  "A simple iterative collection statement"
  `(make-array (length ,vector) :initial-contents
               (loop for ,index from 0 to (1- (length ,vector)) collect 
                     (let ((,item (aref ,vector ,index))) ,term))))

(defmacro vcollect-times ((index max) term)
  `(make-array ,max :initial-contents (loop for ,index from 1 to ,max collect ,term)))

(defmacro dovector ((item vector &optional return &key (index (gentemp))) term)
  `(dotimes (,index (length ,vector) ,return)
     (let ((,item (aref ,vector ,index)))
       ,term)))

(defun vintersection (vector1 vector2)
  (remove-if-not #'(lambda (element) (vmember element vector2)) vector1))
     

(defun vmember (atom vector)
  (dovector (element vector)
    (if (equal atom element)
        (return T))))

(defun tab (n)
  (dotimes (curr n)
    (format t "   ")))

(defun vector-embedded (vector)
  (if (vectorp vector)
  (dovector (curr vector)
     (if (vectorp curr)
         (return T)))))

(defun vsum (vector)
  (let ((sum 0))
    (dovector (num vector sum)
              (incf sum num))))

(defun vrandom-in (vector)
  (aref vector (random (length vector))))

(defun veql (vector1 vector2)
  (if (vector-embedded vector1)
      (if (not (vmember nil (mapvect #'veql vector1 vector2)))
          T)
    (if (not (vmember nil (mapvect #'eql vector1 vector2)))
        T)))

(defmacro while ((test return) body)
  `(loop
    (if ,test
        ,body
      (return ,return))))

(defun +- (num)
  (if (> (random 10) 5)
      num
    (- num)))

(defun total-elements (vector)
  (if (vector-embedded vector)
      (vapply #'+ (mapvect #'total-elements vector))
    (length vector)))

(defun vector-sizes (vector &optional (tab 0))
  (if (vector-embedded vector)
      (dotimes (n (length vector))
        (progn 
          (format t "~%")
          (tab tab)
          (format t "~A:" n)
          (vector-sizes (aref vector n) (1+ tab))))
    (progn
      (tab tab)
      (format t "~A" (length vector)))))

(defun vemember (atom vector)
  (if (vector-embedded vector)
      (vemember T (mapvect #'(lambda (element) (vemember atom element)) vector))
    (vmember atom vector)))

(defun vector-print (vector &optional (tab 0))
  (if (vector-embedded vector)
      (dotimes (n (length vector))
        (progn
          (format t "~%")
          (tab tab)
          (format t "~A:")
          (vector-print (aref vector n) (1+ tab))))
    (dotimes (n (length vector))
      (progn
        (format t "~%")
        (tab tab)
        (format t "~A: ~A" n (aref vector n))))))

(defun population-print (population)
  (dovector (layer population nil :index laycurr)
     (dovector (pool layer nil :index poolcurr)
         (dovector (neuron pool nil :index neurcurr)
            (format t "~%Layer ~A Pool ~A Neuron ~A ~%~TName ~A~%~TFriends ~A~%~TWeights ~A~%~TTrials ~A~%~TFitness ~A~%~TImprovement ~A"
                    laycurr
                    poolcurr
                    neurcurr
                    (neuron-name neuron)
                    (neuron-friends neuron)
                    (neuron-weights neuron)
                    (neuron-trials neuron)
                    (neuron-fitness neuron)
                    (neuron-improvement neuron))))))
                
(defun network-print (network)
  (dovector (layer network nil :index laycurr)
     (dovector (neuron layer nil :index neurcurr)
         (format t "~%Layer ~A Neuron ~A~%~TName ~A~%~TFriends ~A~%~TWeights ~A~%~TTrials ~A~%~TFitness ~A"
                 laycurr
                 neurcurr
                 (neuron-name neuron)
                 (neuron-friends neuron)
                 (neuron-weights neuron)
                 (neuron-trials neuron)
                 (neuron-fitness neuron)))))


(defun auto-associate (network size many)
  (let* ((patterns (vcollect-times (n many) (binary (1- n) size)))
         (outputs (vcollect-vector (pattern patterns) (network-feedforward network pattern)))
         (differences (mapvect #'(lambda (pattern output)
                                   (vaverage (mapvect #'abs (mapvect #'- pattern output))))
                               patterns outputs))
         (totdiff (vaverage differences)))
    (if (eql totdiff 0.0)
        (saveas "~%Perfect network evolved.  Save network as: " network)
      (/ totdiff))))
    
         
(defun vaverage (vector)
  (/ (vsum vector) (length vector)))
                                          

(defun pattern-error (input output)
  (/ (- (sqrt (length input)) (mse input output)) (sqrt (length input))))

(defun mse (pattern test)
  (sqrt (vapply #'+ (mapvect #'square (mapvect #'- pattern test)))))

(defun square (num)
  (expt num 2))

(defun binary-threshold (num)
  (if (< num 0.5) 0 1))

(defun binary (num &optional (places) (binary #()))
  (if (> num 0)
      (let* ((remainder (mod num 2))
             (quotient (div num 2)))
        (binary quotient places (vcons remainder binary)))
    (if places
        (vfill-left :size places :fill 0 :vector binary)
      binary)))

(defun vfill-left (&key (size)(fill)(vector))
  (if (< (length vector) size)
      (vfill-left :size size :fill fill :vector (vcons fill vector))
    vector))

(defun div (num base)
  (/ (- num (mod num base)) base))

