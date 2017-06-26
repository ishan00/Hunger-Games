#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require "sprite.rkt")
(struct posn (x y) #:extra-constructor-name make-posn)
(struct sprite (type x y theta) #:transparent)
(struct object (type x y storage) #:transparent)
(struct resource (health wood stone) #:transparent)
(struct world (character meter herbs stones trees mobs) #:transparent)

;World
;- character (sprite) :- Type includes sprite-axe,sprite-hammer,sprite-sword, Initial theta = 0
;- meter (resource) :- Stores current value of Health, Wood and Stones, Initially (100,0,0)
;- herbs (list(object)) :- Type include empty, berry. Storage 5
;- stones (list(object)) :- Storage 5
;- trees (list(object)) :- Storage 5
;- mobs (list(object)) :- Types include Rabbit , Arctic Fox , Spider , Dragon. Storage refers to number of attacks before death.

(define background (rectangle 1300 700 "solid" (make-color 0 80 60)))
(define my-world
  (world (sprite (sprite-axe) 50 100 0)
         (resource 100 0 0)
         (list (object "empty" 1250 100 0)
               (object "empty" 1250 130 0)
               (object "empty" 1250 160 0)
               (object "empty" 1250 190 0)
               (object "empty" 1250 220 0)
               (object "empty" 1250 250 0)
               (object "empty" 700 500 0)
               (object "empty" 700 400 0)
               (object "empty" 800 600 0)
               (object "empty" 600 400 0)
               (object "berry" 50 560 5)
               (object "berry" 50 590 5)
               (object "berry" 50 620 5)
               (object "berry" 50 650 5)
               (object "berry" 400 100 5)
               (object "berry" 450 200 5)
               (object "berry" 20 30 5)
               (object "berry" 1000 300 5)
               (object "berry" 400 100 5)
               (object "berry" 450 200 5)
               (object "berry" 20 30 5)
               (object "berry" 1000 300 5)
               (object "berry" 1250 500 5))
         (list (object "stone" 1100 100 5)
               (object "stone" 800 100 5)
               (object "stone" 700 50 5)
               (object "stone" 400 300 5)
               (object "stone" 550 550 5)
               (object "stone" 300 100 5)
               (object "stone" 600 200 5)
               (object "stone" 800 0 5)
               (object "stone" 850 200 5)
               (object "stone" 300 100 5))
         (list (object "tree" 100 400 5)
               (object "tree" 100 400 5)
               (object "tree" 100 400 5)
               (object "tree" 100 400 5)
               (object "tree" 100 400 5)
               (object "tree" 100 400 5)
               (object "tree" 100 400 5)
               (object "tree" 100 400 5)
               (object "tree" 100 400 5))
         (list (object "rabbit" 200 80 5)
               (object "rabbit" 800 350 5)
               (object "rabbit" 30 100 5))))

(define (string->img str)
  (cond ((equal? str "empty") (empty-herbs))
        ((equal? str "berry") (berry-herbs))
        ((equal? str "stone") (stones))
        ((equal? str "rabbit") (rabbit))
        ((equal? str "tree") (tree))))

(define (draw list background)
  (if (null? list) background
      (let* ((obj (car list)))
        (begin
          (set! background (place-image (string->img (object-type obj))
                                        (object-x obj)
                                        (object-y obj)
                                        background))
          (draw (cdr list) background)))))

(define (meters h w s)
  (add-line
   (add-line
    (add-line
     (add-line
      (add-line
       (add-line
        (rectangle 200 90 "solid" (make-color 0 51 25))
        17 20 183 20
        (make-pen (make-color 0 100 38) 15 "solid" "round" "round"))
       20 20 (+ (* (/ h 100) 160) 20) 20
       (make-pen (make-color 0 200 76) 12 "solid" "round" "round"))
      17 45 183 45
      (make-pen (make-color 100 50 0) 15 "solid" "round" "round"))
     20 45 (+ (* (/ w 100) 160) 20) 45
     (make-pen (make-color 153 76 0) 12 "solid" "round" "round"))
    17 70 183 70
    (make-pen (make-color 60 60 60) 15 "solid" "round" "round"))
   20 70 (+ (* (/ s 100) 160) 20) 70
   (make-pen (make-color 128 128 128) 12 "solid" "round" "round")))
   
   
(define (render-map input-world)
  (place-image (meters (resource-health (world-meter input-world))
                       (resource-wood (world-meter input-world))
                       (resource-stone (world-meter input-world)))
               100
               600
               (place-image (rotate (sprite-theta (world-character input-world))
                                    (sprite-type (world-character input-world)))
                            (sprite-x (world-character input-world))
                            (sprite-y (world-character input-world))
                            (draw (world-mobs input-world)
                                  (draw (world-trees input-world)
                                        (draw (world-stones input-world)
                                              (draw (world-herbs input-world) background)))))))

(define (update-world-on-tick input-world)
  (let* ((new-health (if (= (resource-health (world-meter input-world)) 0) 0
                         (- (resource-health (world-meter input-world)) 1))))
    (world (world-character input-world)
           (resource new-health
                     (resource-wood (world-meter input-world))
                     (resource-stone (world-meter input-world)))
           (world-herbs input-world)
           (world-stones input-world)
           (world-trees input-world)
           (world-mobs input-world))))
         
(define (update-sprite input-world new-pos)
  (world (sprite (sprite-type (world-character input-world))
                 (posn-x new-pos)
                 (posn-y new-pos)
                 (sprite-theta (world-character input-world)))
         (world-meter input-world)
         (world-herbs input-world)
         (world-stones input-world)
         (world-trees input-world)
         (world-mobs input-world)))

(define (key-event input-world a-key)
  (cond ((key=? a-key "up") (update-sprite input-world (make-posn
                                                        (sprite-x (world-character input-world))
                                                        (- (sprite-y (world-character input-world)) 2))))
        ((key=? a-key "down") (update-sprite input-world (make-posn
                                                          (sprite-x (world-character input-world))
                                                          (+ (sprite-y (world-character input-world)) 2))))
        ((key=? a-key "left") (update-sprite input-world (make-posn
                                                          (- (sprite-x (world-character input-world)) 2)
                                                          (sprite-y (world-character input-world)))))
        ((key=? a-key "right") (update-sprite input-world (make-posn
                                                           (+ (sprite-x (world-character input-world)) 2)
                                                           (sprite-y (world-character input-world)))))
        (else input-world)))

(big-bang my-world
          (to-draw render-map)
          (on-tick update-world-on-tick 1/30)
          (on-key key-event))