#lang racket/gui
(provide (all-defined-out))
(require "sprite.rkt")
(require 2htdp/image)
(require 2htdp/universe)
(struct object(type x y) #:transparent #:mutable)

(struct meter(health wood stone) #:transparent #:mutable)
(define timer (current-seconds))

(define sprite ((sprite-hammer) 0))
(define x_pos 50)
(define y_pos 60)
(define theta 0)

(define (incrementX val)
  (set! x_pos (+ x_pos val)))
(define (incrementY val)
  (set! y_pos (+ y_pos val)))

(define resources
  (meter 100 0 0))

(define (stones_list)
  (list (object "stone" 100 -100)
        (object "stone" -400 -10)
        (object "stone" -100 0)
        (object "stone" 400 -100)
        (object "stone" 150 -50)
        (object "stone" 300 100)
        (object "stone" -300 -200)
        (object "stone" 400 0)
        (object "stone" 50 200)
        (object "stone" 300 -100)))

(define (herbs_list)
  (list (object "empty" 100 100)
        (object "empty" -100 200)
        (object "empty" -100 -200)
        (object "empty" 200 100)
        (object "empty" 200 -100)
        (object "berry" 100 200)
        (object "berry" 100 -200)
        (object "berry" 0 0)
        (object "berry" -200 100)
        (object "berry" -200 -100)))

(define (mobs_list)
  (list (object "rabbit" 200 0)
        (object "rabbit" -300 300)
        (object "rabbit" 0 100)
        (object "rabbit" -100 -100)))

(define (draw list fig background)
  (if (null? list) background
      (let* ((obj (car list)))
        (begin
          (set! background (overlay/offset fig (object-x obj) (object-y obj) background))
          (draw (cdr list) fig background)))))

(define (map)
  (overlay/offset sprite
                  x_pos
                  y_pos
                  (draw (mobs_list) (rabbit)
                        (draw (herbs_list) (empty-herbs)
                              (draw (stones_list) (stones) (background))))))
   
(define (background)
  (put-pinhole 0 0 (rectangle 1200 700 "solid" (make-color 0 80 60))))

;(define (angle x y)
;  (cond ((and (< x 0) (> y 0)) (+ pi (atan (/ y x))))
;        ((and (< x 0) (< y 0)) (- (atan (/ y x)) pi))
;        (else (atan (/ y x)))))


(define game (new frame% [label "Game"] [x 100] [y 10] [height 700] [width 1200]))
(define my-canvas%
  (class canvas%
    ;    (define/override (on-event event)
    ;      (when (eq? (send event get-event-type) 'left-down)
    ;        (let* ((x (send event get-x))
    ;               (y (send event get-y)))
    ;          (set! theta (angle (- x x_sprite) (- y y_sprite)))
    ;          (send canvas refresh))))
    (define/override (on-char event)
      (let* ((key (send event get-key-code)))
        (cond ((eq? key 'right) (incrementX -5))
              ((eq? key 'left) (incrementX 5))
              ((eq? key 'up) (incrementY 5))
              ((eq? key 'down) (incrementY -5))
              ((eq? key #\space) (begin
                                   (set! sprite ((sprite-sword) 45))
                                   (send canvas refresh)
                                   (sleep/yield 1)
                                   (set! sprite ((sprite-sword) 0)))))
        (send canvas refresh)))
    (super-new)))
(define canvas (new my-canvas% [parent game]
                    [paint-callback (λ (c d)
                                      (send d draw-bitmap (image->bitmap (map)) 0 0))]))

(define (image->bitmap image)
  (let* ([width (image-width image)]
         [height (image-height image)]
         [bm (make-object bitmap% width height)]
         [dc (make-object bitmap-dc% bm)])
    (send dc clear)
    (send image draw dc 0 0 0 0 width height 0 0 #f)
    bm))

;(define (action)
;  (begin
;    (set! sprite ((sprite-axe) 20))
;    (send canvas refresh)
;    (set! sprite ((sprite-axe) 45))
;    (send canvas refresh)
;    (set! sprite ((sprite-axe) 0))
;    (send canvas refresh)))

(define health (new gauge% [label "Health"]
                    [parent game]
                    [min-width 50]
                    [stretchable-width 0]
                    [range 100]))
(define stone (new gauge% [label "Stone"]
                   [parent game]
                   [range 200]))
(define wood (new gauge% [label "Wood"]
                  [parent game]
                  [range 200]))
(send health set-value 100)

(big-bang 10
 (on-tick
 (set! resources (meter (- (meter-health resources) 1) (meter-wood resources) (meter-stone resources))) 1)
 (to-draw (map)))

(send game show #t)