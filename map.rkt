#lang racket/gui
(require "sprite.rkt")
(require 2htdp/image)
(define game (new frame% [label "Game"] [x 350] [y 50] [height 600] [width 600]))
(define my-canvas%
  (class canvas%
    (define/override (on-event event)
      (when (eq? (send event get-event-type) 'motion)
          (let* ((x (send event get-x))
                 (y (send event get-y)))
            ;(print (angle x y))
            (map))))
    (define/override (on-char event)
      (let* ((key (send event get-key-code)))
        (cond ((eq? key 'right) (print "Right"))
              ((eq? key 'left) (print "Left"))
              ((eq? key 'up) (print "Up"))
              ((eq? key 'down) (print "Down")))))
    (super-new)))
(define canvas (new my-canvas% [parent game]
                    [paint-callback (λ (c d)
                                           (send d draw-bitmap (map) 0 0))]))

(send game show #t)

(define (map)
  (overlay/offset
   (sprite)
   40 50
  (background)))
(define (background)
  (rectangle 6000 6000 "solid" (make-color 0 200 0)))
(define (angle x y)
  (cond ((and (< x 0) (> y 0)) (+ pi (atan (/ y x))))
        ((and (< x 0) (< y 0)) (- (atan (/ y x)) pi))
        (else (atan (/ y x)))))