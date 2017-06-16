#lang racket/gui
(require "sprite.rkt")
(require 2htdp/image)

(define x_sprite 40)
(define y_sprite 50)
(define theta 0)
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
        (cond ((eq? key 'right) (set! x_sprite (- x_sprite 5)))
              ((eq? key 'left) (set! x_sprite (+ x_sprite 5)))
              ((eq? key 'up) (set! y_sprite (+ y_sprite 5)))
              ((eq? key 'down) (set! y_sprite (- y_sprite 5))))
        (send canvas refresh)))
    (super-new)))
(define canvas (new my-canvas% [parent game]
                    [paint-callback (λ (c d)
                                      (send d draw-bitmap (image->bitmap (map)) 0 0))]))

(send game show #t)

(define (map)
  (overlay/offset
   (empty-herbs)
   -200 -100
   (overlay/offset
    (empty-herbs)
    200 -100
    (overlay/offset
     (empty-herbs)
     -200 100
     (overlay/offset
      (empty-herbs)
      200 100
      (overlay/offset
       (empty-herbs)
       0 0
       (overlay/offset
        (empty-herbs)
        -100 -200
        (overlay/offset
         (empty-herbs)
         100 -200
         (overlay/offset
          (empty-herbs)
          -100 200
          (overlay/offset
           (empty-herbs)
           100 200
           (overlay/offset
            (empty-herbs)
            100 100
            (overlay/offset
             (stones)
             300 -100
             (overlay/offset
              (stones)
              50 200
              (overlay/offset
               (stones)
               400 0
               (overlay/offset
                (stones)
                -300 -200
                (overlay/offset
                 (stones)
                 300 100
                 (overlay/offset
                  (stones)
                  150 -50
                  (overlay/offset
                   (stones)
                   400 -100
                   (overlay/offset
                    (stones)
                    -100 0
                    (overlay/offset
                     (stones)
                     -400 -10
                     (overlay/offset
                      (stones)
                      100 -100
                      (overlay/offset
                       (rotate (/ (* -180 theta) pi) (sprite))
                       x_sprite y_sprite
                       (background)))))))))))))))))))))))
   
(define (background)
  (put-pinhole 0 0 (rectangle 1200 700 "solid" (make-color 0 80 60))))
(define (angle x y)
  (cond ((and (< x 0) (> y 0)) (+ pi (atan (/ y x))))
        ((and (< x 0) (< y 0)) (- (atan (/ y x)) pi))
        (else (atan (/ y x)))))

(define (image->bitmap image)
  (let* ([width (image-width image)]
         [height (image-height image)]
         [bm (make-object bitmap% width height)]
         [dc (make-object bitmap-dc% bm)])
    (send dc clear)
    (send image draw dc 0 0 0 0 width height 0 0 #f)
    bm))