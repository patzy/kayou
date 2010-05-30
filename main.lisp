(in-package #:kayou)

(defvar *screen-width* 1024)
(defvar *screen-height* 768)
(defvar *view* (glaw:create-2d-view 0 0 *screen-width* *screen-height*))
(defvar *font* nil)
(defvar *music-volume* 1.0)
(defvar *sfx-volume* 0.75)
(defvar *stack* (glaw:make-screen-stack))
(defvar *running* t)
(defvar *high-scores* nil)

;;; high score table
(defun load-scores ()
  (with-open-file (f #P"scores" :direction :input)
    (setf *high-scores* (read f)))
  (format t "Loaded high score table: ~S~%" *high-scores*))

(defun save-scores ()
  (with-open-file (f #P"scores" :direction :output :if-exists :overwrite)
    (format f "~S" *high-scores*)))

(defun render-score-table (x y)
  (glaw:format-at x y *font* "BEST SCORES:")
  (decf y 40) (incf x 15)
  (loop for (name . score) in *high-scores*
     do (glaw:format-at x y *font* "~A : ~D" name score)
       (decf y 30)))

(defun high-score-p (score)
  (some (lambda (item)
           (< (cdr item) score)) *high-scores*))


(defun insert-high-score (name score)
  (push (cons name score) *high-scores*)
  (sort *high-scores* #'> :key #'cdr)
  (setf *high-scores* (butlast *high-scores*)))

;;; Game entities
(defstruct entity
  (pos (glaw:make-vector-2d :x 0.0 :y 0.0))
  (speed (glaw:make-vector-2d :x 0.0 :y 0.0))
  shape)

(defmethod render ((it entity))
  (glaw:set-color/rgb 1.0 1.0 1.0 1.0)
  (glaw:render-shape (entity-shape it)))

(defmethod update ((it entity) dt)
  (let ((old-pos (entity-pos it)))
    (setf (entity-pos it) (glaw:vec-sum (entity-pos it) (glaw:vec-scale (entity-speed it) dt)))
    (when (> 0 (glaw:vector-2d-x (entity-pos it)))
      (setf (glaw:vector-2d-x (entity-pos it)) (float *screen-width*)))
    (when (< *screen-width* (glaw:vector-2d-x (entity-pos it)))
      (setf (glaw:vector-2d-x (entity-pos it)) 0.0))
    (when (> 0 (glaw:vector-2d-y (entity-pos it)))
    (setf (glaw:vector-2d-y (entity-pos it)) (float *screen-height*)))
    (when (< *screen-height* (glaw:vector-2d-y (entity-pos it)))
      (setf (glaw:vector-2d-y (entity-pos it)) 0.0))
    (let ((dp (glaw:vec-diff old-pos (entity-pos it))))
      (glaw:translate-shape (entity-shape it) (glaw:vector-2d-x dp) (glaw:vector-2d-y dp) 0.0))))

;; Bullet
(defstruct (bullet (:include entity))
  (spawn-date (get-internal-real-time)))

(defun create-bullet (x y speed angle)
  (let ((bul (make-bullet :pos (glaw:make-vector-2d :x x :y y)
                          :speed (glaw:vec-rotate (glaw:make-vector-2d :x speed :y 0.0)
                                                  angle)
                          :shape (glaw:create-circle-shape 0.0 0.0 3))))
    (glaw:translate-shape (bullet-shape bul) x y 0)
    bul))

(defmethod render ((it bullet))
  (glaw:set-color/rgb 1.0 0.0 0.0 1.0)
  (glaw:render-shape (bullet-shape it))
  (glaw:set-color/rgb 1.0 1.0 1.0 1.0))

;; Ship
(defstruct (ship (:include entity))
  pew
  (angle 0.0)
  (thrust 0.0)
  (turning nil)
  (firing nil))

(defun create-ship ()
  (let ((ship (make-ship :pos (glaw:make-vector-2d :x (/ *screen-width* 2.0)
                                                   :y (/ *screen-height* 2.0))
                       :shape (glaw:create-triangle-shape -5.0 4.0 -5.0 -4.0 5.0 0.0 :filled nil))))
    (glaw:translate-shape (ship-shape ship) (glaw:vector-2d-x (ship-pos ship))
                                            (glaw:vector-2d-y (ship-pos ship)) 0.0)
    ship))

(defmethod render ((it ship))
  (glaw:set-color/rgb 1.0 1.0 1.0 1.0)
  (glaw:render-shape (ship-shape it))
  (gl:color 1.0 0.0 0.0 1.0)
  (gl:begin :lines)
  (gl:vertex (glaw:vector-2d-x (ship-pos it)) (glaw:vector-2d-y (ship-pos it)))
  (gl:vertex (+ (glaw:vector-2d-x (ship-pos it))
                (* (ship-thrust it) (cos (glaw:deg->rad (ship-angle it)))))
             (+ (glaw:vector-2d-y (ship-pos it))
                (* (ship-thrust it) (sin (glaw:deg->rad (ship-angle it))))))
  (gl:end))

(defmethod update ((it ship) dt)
  (let ((dangle (* 360 dt)))
    (case (ship-turning it)
      (:left (incf (ship-angle it) dangle)
             (glaw:rotate-shape-2d (ship-shape it) (glaw:deg->rad dangle)
                              (glaw:vector-2d-x (ship-pos it)) (glaw:vector-2d-y (ship-pos it))))
      (:right (decf (ship-angle it) dangle)
              (glaw:rotate-shape-2d (ship-shape it) (- (glaw:deg->rad dangle))
                               (glaw:vector-2d-x (ship-pos it)) (glaw:vector-2d-y (ship-pos it))))))
  (incf (glaw:vector-2d-x (ship-speed it)) (* (ship-thrust it) dt
                                                (cos (glaw:deg->rad (ship-angle it)))))
  (incf (glaw:vector-2d-y (ship-speed it)) (* (ship-thrust it) dt
                                                (sin (glaw:deg->rad (ship-angle it)))))
  (call-next-method))

(glaw:key-handler (it ship) (:left :press)
   (setf (ship-turning it) :left))

(glaw:key-handler (it ship) (:left :release)
   (setf (ship-turning it) nil))

(glaw:key-handler (it ship) (:right :press)
   (setf (ship-turning it) :right))

(glaw:key-handler (it ship) (:right :release)
   (setf (ship-turning it) nil))

(glaw:key-handler (it ship) (:up :press)
   (setf (ship-thrust it) 100))

(glaw:key-handler (it ship) (:up :release)
   (setf (ship-thrust it) 0))

(glaw:key-handler (it ship) (:down :press)
   (setf (ship-thrust it) -100))

(glaw:key-handler (it ship) (:down :release)
   (setf (ship-thrust it) 0))

(glaw:key-handler (it ship) (:space :press)
   (setf (ship-firing it) t))

(glaw:key-handler (it ship) (:space :release)
   (setf (ship-firing it) nil))

;; Rock
;; FIXME: make sure rocks are convex when generating their shapes
(defstruct (rock (:include entity))
  radius)

(defun create-rock (&optional (x (random *screen-width*))
                              (y (random *screen-height*))
                              (radius 70.0))
  (let ((rock (make-rock :pos (glaw:make-vector-2d :x (float x) :y (float y))
                         :radius radius
                         :speed (glaw:make-vector-2d :x (glaw:random-between -100.0 100.0)
                                                     :y (glaw:random-between -100.0 100.0)))))
    (setf (rock-shape rock)
          (glaw:create-polygon-shape (loop for angle below 360 by 18
                                        with dispersion = (* 0.1 radius)
                                        for length = (glaw:random-between (- radius dispersion)
                                                                          (+ radius dispersion))
                                        collect (list (* length (cos (glaw:deg->rad angle)))
                                                      (* length (sin (glaw:deg->rad angle)))))))
    (glaw:translate-shape (rock-shape rock) (float x) (float y) 0)
    rock))

(defmethod render ((it rock))
  (glaw:set-color/rgb .32 .32 .32 1.0)
  (glaw:render-shape (rock-shape it) :polygon)
  (glaw:set-color/rgb 0.12 0.12 0.12 1.0)
  (gl:line-width 3.0)
  (glaw:render-shape (rock-shape it) :line-loop)
  (gl:line-width 1.0))

;;; Game screens
(defstruct title-screen
  big-font
  small-font
  tiny-font
  rocks
  music)

(defmethod glaw:init-screen ((it title-screen) &key)
  (glaw:push-input-handlers)
  (glaw:add-input-handler it)
  (setf (title-screen-big-font it) (glaw:use-resource "title-font")
        (title-screen-small-font it) (glaw:use-resource "font")
        (title-screen-tiny-font it) (glaw:use-resource "copyright-font")
        (title-screen-music it) (glaw:use-resource "title-music")
        (title-screen-rocks it) (list (create-rock (random *screen-width*)
                                                   (random *screen-height*)
                                                   (random 70.0))
                                      (create-rock (random *screen-width*)
                                                   (random *screen-height*)
                                                   (random 70.0))
                                      (create-rock (random *screen-width*)
                                                   (random *screen-height*)
                                                   (random 70.0))
                                      (create-rock (random *screen-width*)
                                                   (random *screen-height*)
                                                   (random 70.0))
                                      (create-rock (random *screen-width*)
                                                   (random *screen-height*)
                                                   (random 70.0))))
  (glaw:resume-screen it))

(defmethod glaw:shutdown-screen ((it title-screen))
  (glaw:remove-input-handler it)
  (glaw:pop-input-handlers)
  (glaw:drop-resource "title-music")
  (glaw:drop-resource "title-font")
  (glaw:drop-resource "font")
  (glaw:drop-resource "copyright-font"))

(defmethod glaw:resume-screen ((it title-screen))
  (glaw:play-sound (title-screen-music it) :loop t))

(defmethod glaw:suspend-screen ((it title-screen))
  (glaw:stop-sound (title-screen-music it)))

(defmethod glaw:render-screen ((it title-screen))
  (dolist (r (title-screen-rocks it))
    (render r))
  (glaw:set-color/rgb 1 1 1 1)
  (render-score-table (/ *screen-width* 2.0) (* (/ *screen-height* 3.0) 2))
  (glaw:render-wrapped-string 0 (/ *screen-height* 2.0) (/ *screen-width* 3.0)
                              (title-screen-big-font it)
                              "KAYOU"
                              :justify :center)
  (glaw:render-wrapped-string 0 (- (/ *screen-height* 2.0) 20)  (/ *screen-width* 3.0)
                              (title-screen-small-font it)
                              "Press SPACE to play or ESC to quit."
                              :justify :left)
  (glaw:render-wrapped-string (/ *screen-width* 2.0)
                              40
                              (/ *screen-width* 4.0)
                              (title-screen-tiny-font it)
                             "Musics by KKSlider60 and JP Neufeld (Rig) (http://www.newgrounds.com)"
                              :justify :left))

(defmethod glaw:update-screen ((it title-screen) dt)
  (dolist (r (title-screen-rocks it))
    (update r dt)))

(glaw:key-handler (it title-screen) (:space :press)
    (glaw:push-screen (make-game-screen) *stack*))

(glaw:key-handler (it title-screen) (:escape :press)
    (shutdown))

(defstruct game-screen
  score
  ship
  bullets
  rocks
  (time 0.0)
  ;; sound
  pew music explosion)

(glaw:key-handler (it game-screen) (:escape :press)
  (glaw:pop-screen *stack*))

(glaw:key-handler (it game-screen) (:p :press)
  (glaw:push-screen (make-pause-screen) *stack*
                    :propagate-rendering t))

(defmethod glaw:init-screen ((it game-screen) &key)
  (setf (game-screen-score it) 0
        (game-screen-ship it) (create-ship)
        (game-screen-bullets it) '()
        (game-screen-rocks it) (list (create-rock) (create-rock) (create-rock))
        (game-screen-pew it) (glaw:use-resource "pew")
        (game-screen-music it) (glaw:use-resource "game-music")
        (game-screen-explosion it) (glaw:use-resource "explosion"))
  (glaw:push-input-handlers)
  (glaw:add-input-handler it)
  (glaw:add-input-handler (game-screen-ship it))
  (glaw:play-sound (game-screen-music it) :loop t :volume *music-volume*)
  (glaw:schedule 10.0 20.0 (lambda ()
                            (loop for i below (if (> (game-screen-time it) 50.0)
                                                  (random (floor (/ (game-screen-time it) 10.0)))
                                                  (random 5))
                                 do (push (create-rock) (game-screen-rocks it))))))

(defmethod glaw:shutdown-screen ((it game-screen))
  (glaw:drop-resources "pew" "game-music" "explosion")
  (glaw:remove-input-handler it)
  (glaw:remove-input-handler (game-screen-ship it))
  (glaw:pop-input-handlers)
  (glaw:stop-sound (game-screen-music it)))

(defmethod glaw:render-screen ((it game-screen))
  (glaw:select-texture nil)
  (render (game-screen-ship it))
  (dolist (b (game-screen-bullets it))
    (render b))
  (dolist (r (game-screen-rocks it))
    (render r))
  (glaw:set-color/rgb 1 1 1 1)
  (glaw:format-at 50 150  *font* "Score: ~a" (game-screen-score it)))

(defmethod glaw:update-screen ((it game-screen) dt)
  (incf (game-screen-time it) dt)
  (update (game-screen-ship it) dt)
  (dolist (b (game-screen-bullets it))
    (let ((date (get-internal-real-time)))
      (if (> (- date (bullet-spawn-date b)) (* internal-time-units-per-second 5.0))
        (game-destroy-bullet it b)
        (update b dt))))
  (dolist (r (game-screen-rocks it))
    (update r dt))
  (game-collide-bullets it)
  (when (ship-firing (game-screen-ship it))
    (game-ship-fire it))
  (when (null (game-screen-rocks it))
    (glaw:replace-screen *stack* (make-game-over-screen :state :win
                                                        :duration (game-screen-time it)
                                                        :score (+ 10000 (game-screen-score it)))))
  (when (game-collide-ship it)
    (glaw:replace-screen *stack* (make-game-over-screen :state :loose
                                                        :duration (game-screen-time it)
                                                        :score (game-screen-score it)))))

(defun game-explode-rock (game rock)
  (let ((pos (rock-pos rock)))
    (unless (< (rock-radius rock) 10)
      (push (create-rock (glaw:vector-2d-x (rock-pos rock)) (glaw:vector-2d-y (rock-pos rock))
                         (/ (rock-radius rock) 2.0)) (game-screen-rocks game))
      (push (create-rock (glaw:vector-2d-x (rock-pos rock)) (glaw:vector-2d-y (rock-pos rock))
                         (/ (rock-radius rock) 2.0)) (game-screen-rocks game)))
    (game-destroy-rock game rock)))

(defun game-destroy-rock (game rock)
  (glaw:play-sound (game-screen-explosion game) :volume *sfx-volume*)
  (setf (game-screen-rocks game) (remove rock (game-screen-rocks game)))
  (incf (game-screen-score game) (floor (/ 10000 (rock-radius rock)))))

(defun game-destroy-bullet (game bullet)
  (setf (game-screen-bullets game) (remove bullet (game-screen-bullets game))))

(let ((last-fire-time (get-internal-real-time)))
  (defun game-ship-fire (game)
    (when (> (- (get-internal-real-time) last-fire-time)
             (* internal-time-units-per-second 0.1))
      (setf last-fire-time (get-internal-real-time))
      (let* ((ship (game-screen-ship game))
             (bul (create-bullet (glaw:vector-2d-x (ship-pos ship))
                                 (glaw:vector-2d-y (ship-pos ship))
                                 (+ (glaw:vec-mag (ship-speed ship)) 300.0)
                               (glaw:deg->rad (ship-angle ship)))))
        (push bul (game-screen-bullets game))
        (glaw:play-sound (game-screen-pew game) :volume *sfx-volume*)))))

(defun game-collide-bullets (game)
  (dolist (b (game-screen-bullets game))
    (dolist (r (game-screen-rocks game))
      (when (glaw:shape-intersect-p (bullet-shape b) (rock-shape r))
        (game-explode-rock game r)
        (game-destroy-bullet game b)))))

(defun game-collide-ship (game)
  (dolist (r (game-screen-rocks game))
    (when (glaw:shape-intersect-p (ship-shape (game-screen-ship game)) (rock-shape r))
      (return-from game-collide-ship t))))


(defstruct pause-screen
  font)

(glaw:key-handler (it pause-screen) (:space :press)
  (glaw:pop-screen *stack*))

(defmethod glaw:init-screen ((it pause-screen) &key)
  (setf (pause-screen-font it) (glaw:use-resource "title-font"))
  (glaw:push-input-handlers)
  (glaw:add-input-handler it))

(defmethod glaw:shutdown-screen ((it pause-screen))
  (glaw:remove-input-handler it)
  (glaw:pop-input-handlers))

(defmethod glaw:render-screen ((it pause-screen))
  (glaw:set-color/rgb 0 0 1 1)
  (glaw:render-wrapped-string 0 (/ *screen-height* 2.0) *screen-width*
                              (pause-screen-font it)
                              "PAUSE"
                              :justify :center))

(defmethod glaw:update-screen ((it pause-screen) dt)
  (declare (ignore it dt)))

(defstruct game-over-screen
  duration
  state
  score
  font)

(glaw:key-handler (it game-over-screen) (:escape :press)
     (glaw:pop-screen *stack*))

(defmethod glaw:init-screen ((it game-over-screen) &key)
  (setf (game-over-screen-font it) (glaw:use-resource "font"))
  (glaw:push-input-handlers)
  (glaw:add-input-handler it)
  (when (high-score-p (game-over-screen-score it))
    (glaw:schedule 0.5 nil
                   #'glaw:push-screen
                   (make-high-score-screen :score (game-over-screen-score it))
                   *stack* :propagate-rendering t)))

(defmethod glaw:shutdown-screen ((it game-over-screen))
  (glaw:drop-resource "font")
  (glaw:remove-input-handler it)
  (glaw:pop-input-handlers))

(defmethod glaw:render-screen ((it game-over-screen))
  (glaw:select-texture nil)
  (case (game-over-screen-state it)
    (:loose (glaw:set-color/rgb 1 0 0 1)
            (glaw:render-wrapped-string 0 (/ *screen-height* 2.0) *screen-width*
                                        (game-over-screen-font it)
                                        "You lost. Press ESC key to continue."
                                        :justify :center))
    (:win (glaw:set-color/rgb 0 1 0 1)
          (glaw:render-wrapped-string 0 (/ *screen-height* 2.0) *screen-width*
                                      (game-over-screen-font it)
                                      "You won, 10K bonus ! Press ESC key to continue."
                                      :justify :center)))
  (glaw:set-color/rgb 1 1 1 1)
  (glaw:render-wrapped-string 0 (- (/ *screen-height* 2.0) 30) *screen-width*
                              (game-over-screen-font it)
                              (format nil "Your score: ~D" (game-over-screen-score it))
                              :justify :center))

(defmethod glaw:update-screen ((it game-over-screen) dt))

(defstruct high-score-screen
  font
  score
  name)

(glaw:key-handler (it high-score-screen) (nil :press)
   (push glaw::key (high-score-screen-name it))
   (when (= (length (high-score-screen-name it)) 3)
     (insert-high-score (format nil "~{~A~}" (reverse (high-score-screen-name it)))
                        (high-score-screen-score it))
     (glaw:pop-screen *stack*)
     (glaw:pop-screen *stack*)))

(defmethod glaw:init-screen ((it high-score-screen) &key)
  (setf (high-score-screen-font it) (glaw:use-resource "font")
        (high-score-screen-name it) '())
  (glaw:push-input-handlers)
  (glaw:add-input-handler it))

(defmethod glaw:shutdown-screen ((it high-score-screen))
  (glaw:remove-input-handler it)
  (glaw:pop-input-handlers))

(defmethod glaw:render-screen ((it high-score-screen))
  (glaw:set-color/rgb 1 1 1 1)
  (glaw:render-wrapped-string 0 (- (/ *screen-height* 2.0) 65) *screen-width*
                              (high-score-screen-font it)
                              "Congratulations ! This is a new high score."
                              :justify :center)
  (glaw:render-wrapped-string 0 (- (/ *screen-height* 2.0) 95) *screen-width*
                              (high-score-screen-font it)
                              (format nil "Enter 3 letters: ~{~A~}" (reverse
                                                                    (high-score-screen-name it)))
                              :justify :center))

(defmethod glaw:update-screen ((it high-score-screen) dt)
  (declare (ignore it dt)))

;;; Main code
(defun init ()
  (load-scores)
  (glaw:init-sound)
  (glaw:init-content-manager #P"data/")
  (glaw:load-asset "pew.wav" :sound "pew")
  (glaw:load-asset "explosion-01.wav" :sound "explosion")
  (glaw:load-asset "apeiron.wav" :sound "title-music")
  (glaw:load-asset "zombies.wav" :sound "game-music")
  (glaw:load-asset "alien-26.fnt" :fonttool-bitmap-font "font")
  (glaw:load-asset "alien-50.fnt" :fonttool-bitmap-font "title-font")
  (glaw:load-asset "dejavu-sans.fnt" :fonttool-bitmap-font "copyright-font")
  (setf *font* (glaw:use-resource "font"))
  (glaw:push-screen (make-title-screen) *stack*))

(defun shutdown ()
  (glaw:pop-screen *stack*)
  (glaw:dispose-asset "font")
  (glaw:dispose-asset "pew")
  (glaw:dispose-asset "explosion")
  (glaw:dispose-asset "title-music")
  (glaw:dispose-asset "game-music")
  (glaw:shutdown-content-manager)
  (glaw:shutdown-sound)
  (save-scores)
  (setf *running* nil))

(defun draw ()
  (glaw:set-view-2d *view*)
  (glaw:begin-draw)
  (glaw:render-screens *stack*)
  (glaw:end-draw))

(let ((last-update-time (get-internal-real-time)))
  (defun idle ()
    (let* ((elapsed-time (- (get-internal-real-time)
                            last-update-time))
           (dt (/ (* elapsed-time 1.0)
                  internal-time-units-per-second)))
      (glaw:update-sound)
      (glaw:update-scheduler dt)
      (glaw:update-screens *stack* dt)
      (setf last-update-time (get-internal-real-time)))))

(defmethod glop:on-key (window pressed keycode keysym string)
  (glaw:dispatch-key-event keysym (if pressed :press :release) keycode string))

(defmethod glop:on-close (window)
  (shutdown))

(defmethod glop:on-button (window pressed button)
  (glaw:dispatch-button-event :mouse (glaw:translate-mouse-button button)
                              (if pressed :press :release)))

(defmethod glop:on-mouse-motion (window x y dx dy)
  (glaw:update-mouse-position x y)
  (glaw:dispatch-motion-event :mouse dx dy))

(defmethod glop:on-draw (window)
  (draw)
  (glop:swap-buffers window))

(defmethod glop:on-resize (window w h)
  (glaw:reshape w h)
  (setf *screen-width* w *screen-height* h)
  (glaw:update-2d-view *view* 0 0 *screen-width* *screen-height*)
  (draw)
  (glop:swap-buffers window))


(defun run ()
  ;; how to get extensions
  (setf cl-opengl-bindings:*gl-get-proc-address* 'glop:gl-get-proc-address)
  (glop:with-window (win "Kayou" *screen-width* *screen-height*)
    (glaw:setup-gl-defaults)
    (gl:clear-color 0.0 0.0 0.0 1.0)
    (glaw:reshape *screen-width* *screen-height*)
    (init)
    (loop while (and *running* (glop:dispatch-events win :blocking nil)) do
         (idle)
         (draw)
         (glop:swap-buffers win)))
  (setf *running* t))

