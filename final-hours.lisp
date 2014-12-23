;;;; final-hours.lisp

(in-package #:final-hours)

(defparameter *data-root* "src/lisp/final-hours/")
(defparameter *font-root* (merge-pathnames "fonts/" *data-root*))
(defparameter *audio-root* (merge-pathnames "audio/" *data-root*))
(defparameter *gfx-root* (merge-pathnames "gfx/" *data-root*))

;;;; Game Params
(defparameter *game-width* 800)
(defparameter *game-height* 600)
(defparameter *game-state* 0) ; 0=menu, 1:ready, 2:in-game, 3:win-lose

(defparameter *player-missiles* nil)
(defparameter *player-missiles-explosion* nil)
(defparameter *player-missile-count* 0)
(defparameter *player-score* 0)

(defparameter *enemy-missiles* nil)
(defparameter *enemy-missiles-explosion* nil)
(defparameter *enemy-missile-count* 10)
(defparameter *enemies* nil)

(defparameter *level* 1)
(defparameter *wave* 1)
(defparameter *damage* 0)
(defparameter *game-clock* 0)
(defparameter *pause* nil)

;;;; Sound Params
(defparameter *mixer-opened* nil)
(defparameter *music* nil)
(defparameter *music-intro* nil)
(defparameter *soundfx* nil)

;;;; GFX Params
(defparameter *gfx-night-bg* (merge-pathnames "night_bg.png" *gfx-root*))

;;;; Font Params
(defparameter *terminus-ttf-12* 
  (make-instance 'SDL:ttf-font-definition
		 :size 12
		 :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))
(defparameter *terminus-ttf-18* 
  (make-instance 'SDL:ttf-font-definition
		 :size 18
		 :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))
(defparameter *terminus-ttf-24* 
  (make-instance 'SDL:ttf-font-definition
		 :size 24
		 :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))
(defparameter *terminus-ttf-32* 
  (make-instance 'SDL:ttf-font-definition
		 :size 32
		 :filename (merge-pathnames "TerminusTTF.ttf" *font-root*)))

(defparameter *ttf-font-small* nil)
(defparameter *ttf-font-normal* nil)
(defparameter *ttf-font-large* nil)
(defparameter *ttf-font-huge* nil)


;;;;;;;;;;;;;;;;;;;;;;;; CLASSES ;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct player-missile
  (xt 0)
  (yt 0)
  (x0 0)
  (y0 0)
  (x1 0)
  (y1 0)
  (dx 0)
  (dy 0))


(defstruct missile-explosion
  (x 0)
  (y 0)
  (r 0)
  (rt 0)
  (dr 0))


(defstruct enemy-missile
  (xt 0)
  (yt 0)
  (x0 0)
  (y0 0)
  (x1 0)
  (y1 0)
  (dx 0)
  (dy 0)
  (smart 0))



;;;;;;;;;;;;;;;;;;;;;;;; SLIME ;;;;;;;;;;;;;;;;;;;;;;;;

;;;; CONTINUABLE macro

(defmacro continuable (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))


;;;; UPDATE-SWANK function

(defun update-swank ()
  (continuable
   (let ((connection (or swank::*emacs-connection*
			 (swank::default-connection))))
     (when connection
       (swank::handle-requests connection t)))))


;;;;;;;;;;;;;;;;;;;;;;;; PRIMITIVES ;;;;;;;;;;;;;;;;;;;;;;;;

;;;; SQUARE function

(defun square (x)
  (* x x))


;;;; DRAW-TEXT function

(defun draw-text (string x y r g b &optional (font *ttf-font-normal*))
  (sdl:draw-string-solid-* string
			   x y
			   :color (sdl:color :r r :g g :b b)
			   :font font))


;;;; DRAW-BOX function

(defun draw-box (x y w h r g b)
  (sdl:draw-box (sdl:rectangle-from-midpoint-* x y w h)
		:color (sdl:color :r r :g g :b b)))


;;;; DRAW-LINE function

(defun draw-line (x0 y0 x1 y1 r g b)
  (sdl:draw-line-* x0 y0 x1 y1
		   :color (sdl:color :r r :g g :b b)))


;;;; DRAW-CIRCLE function

(defun draw-circle (x y rad r g b)
  (sdl:draw-circle-* x y rad
		     :color (sdl:color :r r :g g :b b)))


;;;; DRAW-CIRCLE-FILLED function

(defun draw-circle-filled (x y rad r g b)
  (sdl:draw-filled-circle-* x y rad
		     :color (sdl:color :r r :g g :b b)))


;;;; DRAW-ELLIPSE-FILLED function

(defun draw-ellipse-filled (x y rx ry r g b)
  (sdl:draw-filled-ellipse-* x y rx ry
		     :color (sdl:color :r r :g g :b b)))


;;;; DRAW-POLYGON function

(defun draw-polygon (vertices r g b)
  (sdl:draw-filled-polygon vertices :color (sdl:color :r r :g g :b b)))


;;;; DRAW-CROSS-HAIR function

(defun draw-cross-hair (x y r g b)
  ; top
  (draw-box x (- y 10) 2 10 r g b)
  ; right
  (draw-box (+ x 10) y 10 2 r g b)
  ; bottom
  (draw-box x (+ y 10) 2 10 r g b)
  ; left
  (draw-box (- x 10) y 10 2 r g b))


;;;; PLAY-SOUND function

(defun play-sound (s)
  (sdl-mixer:play-sample (aref *soundfx* s)))


;;;; PLAY-SOUND-EXPLOSION function

(defun play-sound-explosion ()
  (play-sound (random 6)))


;;;; PLAY-SOUND-MISSILE function

(defun play-sound-missile ()
  (play-sound (+ (random 3) 6)))


;;;;;;;;;;;;;;;;;;;;;;;; LEVEL ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; DISPLAY-LEVEL function

(defun display-level ()
  (sdl:draw-surface-at-* (sdl:load-image *gfx-night-bg*) 0 0))


;;;; DRAW-MOUNTAINS function

(defun draw-mountains ()
  (draw-polygon '((0 520) (20 520) (70 540) (80 540) (160 480) (220 520) (280 520) (330 530) (340 530) (360 480) (380 520) (400 520) (480 460) (520 500) (540 500) (580 500) (600 540) (620 540) (680 500) (760 500) (800 520) (800 600) (0 600)) 0 0 0))


;;;; DRAW-GAME-UI function

(defun draw-game-ui ()
  (if (<= *game-clock* (* 60 2))
      (progn (draw-text (format nil "Level ~a" *level*)
			380 280 255 255 255 *ttf-font-Large*)
	     (draw-text (format nil "Wave ~a" *wave*)
			385 320 255 255 255 *ttf-font-Large*)))

  (draw-text (format nil "Damage: ~a%" *damage*) 
	     20 580 255 255 255 *ttf-font-small*)

  (draw-text (format nil "Score: ~a" *player-score*) 
	     380 580 255 255 255 *ttf-font-small*)

  (draw-text (format nil "Missiles: ~a" *player-missile-count*) 
	     700 580 255 255 255 *ttf-font-small*)

  (if (eql *pause* t)
      (draw-text "Paused" 
	     380 280 255 255 255 *ttf-font-large*)))


;;;;;;;;;;;;;;;;;;;;;;;; ENEMY ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; CREATE-ENEMIES-ATTACK-SCHEDULE function

(defun create-enemies-attack-schedule ()
  (loop for x below *enemy-missile-count*
     do (push (random (* 60 60)) *enemies*)))


;;;; CREATE-ENEMIES function

(defun create-enemies ()
  (loop for e in *enemies*
     do (if (<= e *game-clock*)
	    (progn (create-enemy-missiles)
		   (setf *enemies* (remove e *enemies*))))))


;;;; ACTIVATE-SMART-MISSILE function

(defun activate-smart-missile (m)
  (let* ((x0 (enemy-missile-x1 m))
	 (y0 (enemy-missile-y1 m))
	 (xt (enemy-missile-xt m))
	 (d 0)
	 (dx 0))
    
    ; left missile
    (setf d (- (- xt x0) (+ (random 50) 50)))
    (if (< d 10)
	(setf d 10))

    (if (zerop d)
	(setf dx 0)
	(setf dx (/ 1 (/ (- 500 y0) d))))

    (push (make-enemy-missile
	   :xt xt :yt 500
	   :x0 x0 :y0 y0	
	   :x1 x0 :y1 y0
	   :dx dx :dy 1
	   :smart 0)
	  *enemy-missiles*)


    ; right missile
    (setf d (+ (- xt x0) (+ (random 50) 50)))
    (if (> d 790)
	(setf d 790))

    (if (zerop d)
	(setf dx 0)
	(setf dx (/ 1 (/ (- 500 y0) d))))

    (push (make-enemy-missile
	   :xt xt :yt 500
	   :x0 x0 :y0 y0	
	   :x1 x0 :y1 y0
	   :dx dx :dy 1
	   :smart 0)
	  *enemy-missiles*))

  (setf (enemy-missile-smart m) 0))


;;;; CREATE-ENEMY-MISSILES function

(defun create-enemy-missiles ()
  (let* ((xt (+ (random 780) 10))
	 (x0 (+ (random 700) 50))
	 (d (- xt x0))
	 (dx 0)
	 (smart 0))

    (if (zerop d)
	(setf dx 0)
	(setf dx (/ 1 (/ 500 d))))

    (if (>= *level* 3)
	(if (<= (+ (random 100)) (* 3 *level*))
	    (setf smart 1)))

    (push (make-enemy-missile
	   :xt xt :yt 500
	   :x0 x0 :y0 0	
	   :x1 x0 :y1 0
	   :dx dx :dy 1
	   :smart smart)
	  *enemy-missiles*)))


;;;; UPDATE-ENEMY-MISSILES function

(defun update-enemy-missiles ()
  (loop for m in *enemy-missiles*
     do (if (>= (enemy-missile-y1 m) (enemy-missile-yt m))
	    (progn (setf *enemy-missiles* (remove m *enemy-missiles*))
		   (create-enemy-explosion (enemy-missile-xt m) (enemy-missile-yt m))
		   (play-sound-explosion))
	    (progn (setf (enemy-missile-x1 m) (+ (enemy-missile-x1 m) (enemy-missile-dx m)))
		   (setf (enemy-missile-y1 m) (+ (enemy-missile-y1 m) (enemy-missile-dy m)))
		   (if (and (= (enemy-missile-smart m) 1)
			    (> (enemy-missile-y1 m) (+ (random 100) 100)))
		       (activate-smart-missile m)))))

  (update-enemy-missiles-explosion))


;;;; UPDATE-PLAYER-MISSILES-EXPLOSION function

(defun update-enemy-missiles-explosion ()
  (loop for e in *enemy-missiles-explosion*
     do (if (>= (missile-explosion-r e) (missile-explosion-rt e))
	    (progn (setf *enemy-missiles-explosion* (remove e *enemy-missiles-explosion*))
		   (setf *damage* (+ *damage* (+ (random 5) 20)))
		   (play-sound 9))
	    (progn (setf (missile-explosion-r e) (+ (missile-explosion-r e) (/ 1 2)))
		   (destroy-enemy-missiles (missile-explosion-x e) (missile-explosion-y e)
					   (missile-explosion-r e))))))


;;;; DRAW-ENEMY-MISSILES function

(defun draw-enemy-missiles ()
  (loop for m in *enemy-missiles*
       do (progn (draw-line (round (enemy-missile-x0 m)) (round (enemy-missile-y0 m))
		     (round (enemy-missile-x1 m)) (round (enemy-missile-y1 m))
		     150 0 0)
		 (draw-circle-filled (round (enemy-missile-x1 m)) (round (enemy-missile-y1 m))
				     1 255 255 255))))


;;;; DRAW-PLAYER-MISSILES-EXPLOSION function

(defun draw-enemy-missiles-explosion ()
  (loop for e in *enemy-missiles-explosion*
     do (draw-circle-filled (missile-explosion-x e) (missile-explosion-y e)
			    (round (missile-explosion-r e)) 255 0 0)))


;;;; CREATE-EXPLOSION function

(defun create-enemy-explosion (x y)
  (push (make-missile-explosion
	 :x x :y y :r 1 :rt 40 :dr 1) *enemy-missiles-explosion*))


;;;;;;;;;;;;;;;;;;;;;;;; PLAYER ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; DRAW-PLAYER function

(defun draw-player ()
  (draw-cross-hair (sdl:mouse-x) (sdl:mouse-y) 100 100 100))


;;;; UPDATE-PLAYER-MISSILES function

(defun update-player-missiles ()
  (loop for m in *player-missiles*
     do (if (<= (player-missile-y1 m) (player-missile-yt m))
	    (progn (setf *player-missiles* (remove m *player-missiles*))
		   (create-explosion (player-missile-xt m) (player-missile-yt m))
		   (play-sound-explosion))
	    (progn (setf (player-missile-x1 m) (- (player-missile-x1 m) (player-missile-dx m)))
		   (setf (player-missile-y1 m) (+ (player-missile-y1 m) (player-missile-dy m))))))

  (update-player-missiles-explosion))


;;;; UPDATE-PLAYER-MISSILES-EXPLOSION function

(defun update-player-missiles-explosion ()
  (loop for e in *player-missiles-explosion*
     do (if (>= (missile-explosion-r e) (missile-explosion-rt e))
	    (setf *player-missiles-explosion* (remove e *player-missiles-explosion*))
	    (progn (setf (missile-explosion-r e) (+ (missile-explosion-r e) (/ 1 2)))
		   (destroy-enemy-missiles (missile-explosion-x e) (missile-explosion-y e)
					   (missile-explosion-r e))))))


;;;; DESTROY-ENEMY-MISSILES function

(defun destroy-enemy-missiles (x y r)
  (loop for m in *enemy-missiles*
     do (if (<= (sqrt (+ (square (- (enemy-missile-x1 m) x))
			 (square (- (enemy-missile-y1 m) y))))
		r)
	    (progn (setf *enemy-missiles* (remove m *enemy-missiles*))
		   (create-explosion (round (enemy-missile-x1 m)) 
				     (round (enemy-missile-y1 m)))
		   (play-sound-explosion)
		   (update-score 'missile)))))
		  
	      

;;;; DRAW-PLAYER-MISSILES function

(defun draw-player-missiles ()
  (loop for m in *player-missiles*
       do (progn (draw-line (round (player-missile-x0 m)) (round (player-missile-y0 m))
		     (round (player-missile-x1 m)) (round (player-missile-y1 m))
		     80 80 80)
		 (draw-circle-filled (round (player-missile-x1 m)) (round (player-missile-y1 m))
				     1 255 255 255))))


;;;; DRAW-PLAYER-MISSILES-EXPLOSION function

(defun draw-player-missiles-explosion ()
  (loop for e in *player-missiles-explosion*
     do (draw-circle-filled (missile-explosion-x e) (missile-explosion-y e)
			    (round (missile-explosion-r e)) 255 255 150)))


;;;; FIRE-PRIMARY function

(defun fire-primary (x y)
  (unless (or (zerop *player-missile-count*) (> y 500))
    (let ((d (- x (/ *game-width* 2)))
	  (dx 0))
      (if (zerop d)
	  (setf dx 0)
	  (setf dx (* (/ 1 (/ (- y *game-height*) d)) 4)))
      
      (push (make-player-missile
	     :xt x :yt y
	     :x0 (/ *game-width* 2) :y0 (- *game-height* 25)	
	     :x1 (/ *game-width* 2) :y1 (- *game-height* 25)
	     :dx dx :dy -4)
	    *player-missiles*))
    
    (play-sound-missile)
    (setf *player-missile-count* (decf *player-missile-count*))))


;;;; CREATE-EXPLOSION function

(defun create-explosion (x y)
  (push (make-missile-explosion
	 :x x :y y :r 1 :rt 40 :dr 1) *player-missiles-explosion*))

;;;;;;;;;;;;;;;;;;;;;;;; IN GAME ;;;;;;;;;;;;;;;;;;;;;;;;

;;;; UPDATE-GAME-CLOCK function

(defun update-game-clock ()
  (setf *game-clock* (incf *game-clock*)))


;;;; CHECK-END-OF-WAVE function

(defun check-end-of-wave ()
  (if (> *game-clock* (* 60 70))
      (if (and (eql *enemy-missiles-explosion* nil) (eql *enemy-missiles* nil))
	  (if (= *wave* 3)
	      (new-level)
	      (new-wave)))))


;;;; CHECK-DAMAGE-LEVEL function

(defun check-damage-level ()
  (if (>= *damage* 100)
      (change-game-state)))


;;;; PAUSE-GAME function

(defun pause-game ()
  (if (eql *pause* nil)
      (setf *pause* t)
      (setf *pause* nil)))

	   
;;;; NEW-LEVEL function

(defun new-level ()
  (setf *player-score* (+ *player-score* (* 5 *player-missile-count*) (* 1000 *level*)))
  (create-enemies-attack-schedule)
  (setf *game-clock* 0)
  (setf *wave* 1)
  (setf *damage* (- *damage* 20))
  (if (< *damage* 0)
      (setf *damage* 0))
  (setf *player-missile-count* (+ *player-missile-count* 30))
  (setf *level* (incf *level*)))


;;;; NEW-WAVE function

(defun new-wave ()
  (+ *player-score* (* 5 *player-missile-count*))
  (setf *game-clock* 0)
  (create-enemies-attack-schedule)
  (setf *wave* (incf *wave*))
  (setf *damage* (- *damage* 10))
  (if (< *damage* 0)
      (setf *damage* 0))
  (setf *player-missile-count* (+ *player-missile-count* 15)))


;;;;;;;;;;;;;;;;;;;;;;;; SCORING ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; UPDATE-SCORE function

(defun update-score (unit)
  (cond ((equalp unit 'missile)
	 (setf *player-score* (+ *player-score* 25)))))


;;;;;;;;;;;;;;;;;;;;;;;; SCREENS ;;;;;;;;;;;;;;;;;;;;;;;;

;;;; DISPLAY-END-GAME function

(defun display-end-game ()
  ;(sdl:draw-surface-at-* (sdl:load-image *gfx-game-over*) 0 0)

  (draw-text "FINAL HOURS" 320 20 255 255 0 *ttf-font-huge*)

  (draw-text "GAME OVER!" 320 130 255 255 255 *ttf-font-huge*)

  (draw-text (format nil "YOUR SCORE IS ~a" *player-score*) 
			     260 250 255 0 0 *ttf-font-huge*)

  (draw-text "Press SPACE to Continue..." 270 560 255 255 255))


;;;; DISPLAY-MENU function

(defun display-menu ()
  (draw-text "FINAL HOURS" 320 20 255 255 0 *ttf-font-huge*)
  (draw-text "Press SPACE to Start..." 270 560 255 255 255))


;;;;;;;;;;;;;;;;;;;;;;;; GAME STATE ;;;;;;;;;;;;;;;;;;;;;;;;

;;;; STATE-IN-PLAY function

(defun state-in-play ()
  (update-game-clock)
  (display-level)

  (unless (eql *pause* t)
    (update-player-missiles)
    (update-enemy-missiles)
    (check-end-of-wave)
    (check-damage-level)
    (create-enemies))

  (draw-player)
  (draw-player-missiles)
  (draw-player-missiles-explosion)
  (draw-enemy-missiles)
  (draw-enemy-missiles-explosion)
  (draw-mountains)
  (draw-game-ui))



;;;; CONTINUE-OPTION function

(defun continue-option ()
  (cond ((zerop *game-state*) (change-game-state))
	((= *game-state* 2) (change-game-state))
	(t ())))


;;;; CHANGE-GAME-STATE function

(defun change-game-state ()
  (cond ((zerop *game-state*) 
	 (progn (reset-game)
		(when (sdl-mixer:music-playing-p)
		  (sdl-mixer:Pause-Music)
		  (sdl-mixer:Halt-Music))
		(sdl-mixer:play-music *music* :loop t)
		(setf *game-state* 1)))

	((= *game-state* 1) 
	 (progn (when (sdl-mixer:music-playing-p)
		  (sdl-mixer:Pause-Music)
		  (sdl-mixer:Halt-Music))
		(sdl-mixer:play-music *music-intro* :loop t)
		(setf *game-state* 2)))
	
	((= *game-state* 2) (setf *game-state* 0))
	
	(t ())))

;;;;;;;;;;;;;;;;;;;;;;;; THE GAME ;;;;;;;;;;;;;;;;;;;;;;;;


;;;; RENDER function

(defun render ()
  (update-swank)
  (sdl:clear-display sdl:*black*)

  (cond ((= *game-state* 1) (state-in-play))

	((= *game-state* 2) (display-end-game))

	(t (display-menu)))

  (sdl:update-display))


;;;; RESET-GAME function

(defun reset-game ()
  (setf *player-missile-count* 30)
  (setf *player-missiles* nil)
  (setf *player-missiles-explosion* nil)
  (setf *enemy-missile-count* 10)
  (setf *enemy-missiles* nil)
  (setf *enemy-missiles-explosion* nil)
  (setf *enemies* nil)
  (create-enemies-attack-schedule)
  (setf *game-clock* 0)
  (setf *player-score* 0)
  (setf *level* 1)
  (setf *wave* 1)
  (setf *damage* 0)
  (setf *pause* nil))


;;;; INITIALIZE-GAME function

(defun initialize-game ()
  (setf *game-state* 0))


;;;; SETUP-AUDIO function

(defun setup-audio ()
  (setf *soundfx* (make-array 10))
  (sdl-mixer:init-mixer :mp3)
  (setf *mixer-opened* (sdl-mixer:OPEN-AUDIO :chunksize 1024 :enable-callbacks nil))
  (when *mixer-opened*
    (setf (aref *soundfx* 0) (sdl-mixer:load-sample (sdl:create-path "explosion_1.ogg" *audio-root*)))
    (setf (aref *soundfx* 1) (sdl-mixer:load-sample (sdl:create-path "explosion_2.ogg" *audio-root*)))
    (setf (aref *soundfx* 2) (sdl-mixer:load-sample (sdl:create-path "explosion_3.ogg" *audio-root*)))
    (setf (aref *soundfx* 3) (sdl-mixer:load-sample (sdl:create-path "explosion_4.ogg" *audio-root*)))
    (setf (aref *soundfx* 4) (sdl-mixer:load-sample (sdl:create-path "explosion_5.ogg" *audio-root*)))
    (setf (aref *soundfx* 5) (sdl-mixer:load-sample (sdl:create-path "explosion_6.ogg" *audio-root*)))
    (setf (aref *soundfx* 6) (sdl-mixer:load-sample (sdl:create-path "missile_1.ogg" *audio-root*)))
    (setf (aref *soundfx* 7) (sdl-mixer:load-sample (sdl:create-path "missile_2.ogg" *audio-root*)))
    (setf (aref *soundfx* 8) (sdl-mixer:load-sample (sdl:create-path "missile_3.ogg" *audio-root*)))
    (setf (aref *soundfx* 9) (sdl-mixer:load-sample (sdl:create-path "air_raid_siren.ogg" *audio-root*)))
    (setf *music* (sdl-mixer:load-music (sdl:create-path "siren_1.ogg" *audio-root*)))
    (setf *music-intro* (sdl-mixer:load-music (sdl:create-path "heart_1.ogg" *audio-root*)))
    (sample-finished-action)
    (sdl-mixer:allocate-channels 16)))


;;; SAMPLE-FINISHED-ACTION function

(defun sample-finished-action ()
  (sdl-mixer:register-sample-finished
   (lambda (channel)
     (declare (ignore channel))
     nil)))


;;;; CLEAN-UP function

(defun clean-up ()
  (when *music*
    (when (sdl-mixer:music-playing-p)
      (sdl-mixer:Pause-Music)
      (sdl-mixer:Halt-Music))
    (sdl:Free *music*)
    (setf *music* nil))

  (when *music-intro*
    (when (sdl-mixer:music-playing-p)
      (sdl-mixer:Pause-Music)
      (sdl-mixer:Halt-Music))
    (sdl:Free *music-intro*)
    (setf *music-intro* nil))

  (when (sdl-mixer:sample-playing-p nil)
    (sdl-mixer:pause-sample t)
    (sdl-mixer:Halt-sample :channel t))

  (loop for s below (length *soundfx*)
     do (if (equal (aref *soundfx* s) 0)
	    t
	    (progn (sdl:free (aref *soundfx* s))
		   (setf (aref *soundfx* s) 0))))
  
  (when *mixer-opened*
    (sdl-mixer:Close-Audio t)
    (setf *mixer-opened* nil))
  (sdl-mixer:quit-mixer))


;;;; START function

(defun start ()
  (initialize-game)
  (reset-game)
  (sdl:with-init (sdl:sdl-init-video sdl:sdl-init-audio)
    (sdl:window *game-width* *game-height* :title-caption "Final Hours")
    (setf (sdl:frame-rate) 60)

    (setup-audio)

    (sdl-mixer:play-music *music-intro* :loop t)

    (unless (sdl:initialise-default-font *terminus-ttf-18*)
      (error "FONT-EXAMPLE: Cannot initialize the default font."))

    (setf *ttf-font-small* (sdl:initialise-font *terminus-ttf-12*))
    (setf *ttf-font-normal* (sdl:initialise-font *terminus-ttf-18*))
    (setf *ttf-font-large* (sdl:initialise-font *terminus-ttf-24*))
    (setf *ttf-font-huge* (sdl:initialise-font *terminus-ttf-32*))

    (sdl:show-cursor nil)

    (sdl:with-events ()
      (:quit-event ()
		   (clean-up)
		   t)
      (:key-down-event (:key key)
		       (case key
			 (:sdl-key-p (if (= *game-state* 1)
					 (pause-game)))
			 (:sdl-key-q (if (= *game-state* 1)
					 (change-game-state)))
			 (:sdl-key-space (continue-option))
			 (:sdl-key-escape (sdl:push-quit-event))))
      (:key-up-event (:key key)
		     (case key))
      (:mouse-button-down-event (:x x :y y)
				(if (= *game-state* 1)
				    (cond ((sdl:mouse-left-p) 
					   (fire-primary x y))
					  ((sdl:mouse-right-p)
					   (format t "right button: ~ax~a~%" x y)))))
      (:idle ()
	     ;(when (sdl:get-key-state :sdl-key-up) (move-player 'player 'up))
	     ;(when (sdl:get-key-state :sdl-key-down) (move-player 'player 'down))
	     (render)))))
