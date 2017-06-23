#lang racket/gui
(provide sprite axe sword hammer stones empty-herbs berry-herbs sprite-axe sprite-hammer sprite-sword rabbit)
(require 2htdp/image)
(require racket/mpair)
(require lang/posn)

(define (sprite)
  (scale 0.3 (underlay/align/offset
              'center 'bottom
              (underlay/align/offset
               'center 'bottom
               (overlay/align 'center 'center
                              (rectangle 130 90 "solid" (make-color 204 153 255))
                              (rectangle 140 100 "solid" (make-color 224 224 224)))
               0 5
               (overlay/offset
                (overlay/align 'center 'center
                               (circle 20 "solid" (make-color 204 153 255))
                               (circle 25 "solid" (make-color 224 224 224)))
                200 0
                (overlay/align 'center 'center
                               (circle 20 "solid" (make-color 204 153 255))
                               (circle 25 "solid" (make-color 224 224 224)))))
              0 -18
              (underlay/align/offset
               'center 'top
               (overlay/offset
                (circle 20 "solid" "black")
                64 0
                (circle 20 "solid" "black"))
               -4 6
               (overlay/offset
                (circle 7 "solid" "white")
                64 0
                (circle 7 "solid" "white"))))))
  
(define (axe)
  (define wood
    (put-pinhole 5 5 (overlay/offset
                      (line 54 54 (make-pen (make-color 160 80 0) 8 "solid" "round" "round"))
                      -3 -3
                      (line 70 70 (make-pen (make-color 120 60 0) 12 "solid" "round" "round")))))
  (define stone
    (rotate -40 (put-pinhole 7 20
                             (overlay/offset
                              (triangle/sss 70 70 10 "solid" (make-color 200 200 200))
                              0 6
                              (triangle/sss 90 90 15 "solid" (make-color 100 100 100))))))
  (scale 0.3 (clear-pinhole (overlay/pinhole stone wood))))

(define (empty-herbs)
  (scale 0.3 (add-polygon
              (add-polygon
               (add-polygon 
                (add-polygon
                 (add-polygon
                  (add-polygon
                   (add-polygon 
                    (add-polygon
                     (circle 40 "solid" (make-color 0 153 76))
                     (list (make-posn 72 24)
                           (make-pulled-point 1/6 50 108 34 1/6 -50)
                           (make-posn 70 57))
                     "solid" (make-color 0 153 76))
                    (list (make-posn 55 6)
                          (make-pulled-point 1/6 110 88 -2 1/6 -50)
                          (make-posn 74 27))
                    "solid" (make-color 0 153 76))
                   (list (make-posn 28 10)
                         (make-pulled-point 1/6 80 38 -35 1/6 -80)
                         (make-posn 58 10))
                   "solid" (make-color 0 153 76))
                  (list (make-posn 4 65)
                        (make-pulled-point 1/6 50 -15 30 1/6 -50)
                        (make-posn 30 40))
                  "solid" (make-color 0 153 76))
                 (list (make-posn 20 60)
                       (make-pulled-point 1/4 -50 -10 85 1/4 50)
                       (make-posn 25 90))
                 "solid" (make-color 0 153 76))
                (list (make-posn 32 86)
                      (make-pulled-point 1/4 -50 20 130 1/4 50)
                      (make-posn 56 112))
                "solid" (make-color 0 153 76))
               (list (make-posn 52 111)
                     (make-pulled-point 1/4 -50 74 144 1/4 50)
                     (make-posn 86 107))
               "solid" (make-color 0 153 76))
              (list (make-posn 80 108)
                    (make-pulled-point 1/4 -50 122 108 1/4 50)
                    (make-posn 100 80))
              "solid" (make-color 0 153 76))))
;(define (trees)
(define (stones)
  (scale 0.3 (overlay/align
              "center" "center"
              (rotate -10 (pulled-regular-polygon 40 6 1/2 -5 "solid" (make-color 160 160 160)))
              (rotate 30 (pulled-regular-polygon 50 7 1/2 -5 "solid" (make-color 128 128 128))))))
(define (hammer)
  (define wood
    (put-pinhole 9 9 (overlay/offset
                      (line 54 54 (make-pen (make-color 160 80 0) 8 "solid" "round" "round"))
                      -3 -3
                      (line 70 70 (make-pen (make-color 120 60 0) 12 "solid" "round" "round")))))
  (define stone
    (rotate 45
            (overlay/align
             "center" "center"
             (rectangle 44 22 "solid" (make-color 200 200 200))
             (rectangle 48 26 "solid" (make-color 100 100 100)))))
  (scale 0.3 (clear-pinhole (overlay/pinhole stone wood))))
(define (sword)
  (define handle
    (put-pinhole 11 11
                 (overlay/offset
                  (rotate 90
                          (overlay/offset
                           (line 24 24 (make-pen (make-color 160 80 0) 8 "solid" "round" "round"))
                           -3 -3
                           (line 30 30 (make-pen (make-color 120 60 0) 12 "solid" "round" "round"))))
                  15 15
                  (overlay/offset
                   (line 20 20 (make-pen (make-color 160 80 0) 8 "solid" "round" "round"))
                   -3 -3
                   (line 26 26 (make-pen (make-color 120 60 0) 12 "solid" "round" "round"))))))
  (define blade
    (rotate 135
            (put-pinhole 0 10
                         (polygon
                          (list
                           (make-posn 0 0)
                           (make-posn 0 20)
                           (make-posn 70 22)
                           (make-posn 85 10)
                           (make-posn 70 -2))
                          "solid"
                          (make-color 100 100 100)))))
  (scale 0.3 (clear-pinhole (overlay/pinhole blade handle))))

(define (meat)
  (overlay/align
   "center" "center"
   (pulled-regular-polygon 50 3 1/2 -50 "solid" (make-color 255 102 102))
   (pulled-regular-polygon 30 3 1/2 -30 "solid" (make-color 255 255 255))
   ))

(define (berry-herbs)
  (overlay/offset
   (overlay/offset
    (circle 2 "solid" (make-color 178 102 255))
    1 1
    (circle 4 "solid" (make-color 102 0 102)))
   5 -7 
   (overlay/offset
    (overlay/offset
     (circle 2 "solid" (make-color 178 102 255))
     1 1
     (circle 4 "solid" (make-color 102 0 102)))
    -5 0 
    (overlay/offset
     (overlay/offset
      (circle 2 "solid" (make-color 178 102 255))
      1 1
      (circle 4 "solid" (make-color 102 0 102)))
     5 5
     (empty-herbs)))))

(define (sprite-axe)
  (lambda(x)
    (clear-pinhole (overlay/pinhole (put-pinhole 8 24 (sprite)) (rotate x (put-pinhole 28 21 (axe)))))))
(define (sprite-hammer)
  (lambda(x)
    (clear-pinhole (overlay/pinhole (put-pinhole 8 24 (sprite)) (rotate x (put-pinhole 21 21 (hammer)))))))
(define (sprite-sword)
  (lambda(x)
    (clear-pinhole (overlay/pinhole (put-pinhole 8 24 (sprite)) (rotate x (put-pinhole 25 25 (sword)))))))
(define (rabbit)
  (scale 0.3 (overlay/offset
              (overlay/offset
               (overlay/offset
                (circle 5 "solid" "white")
                40 0
                (circle 5 "solid" "white"))
               3 6
               (overlay/offset
                (circle 13 "solid" "black")
                40 0
                (circle 13 "solid" "black")))
              0 -30
              (add-polygon 
               (add-polygon
                (overlay/align
                 "center" "center"
                 (circle 40 "solid" (make-color 255 153 204))
                 (circle 44 "solid" "white"))
                (list (make-posn 17 20)
                      (make-pulled-point 1/6 50 5 -34 1/6 -50)
                      (make-posn 26 12))
                "solid" (make-color 255 153 204))
               (list (make-posn 61 44)
                     (make-pulled-point 1/6 110 84 2 1/6 -50)
                     (make-posn 70 54))
               "solid" (make-color 255 153 204)))))

;(sprite)
;(axe)
;(hammer)
;(sword)
;(stones)
;(empty-herbs)
;(berry-herbs)
;(sprite-axe)
;(sprite-hammer)
;(sprite-sword)
;(rabbit)