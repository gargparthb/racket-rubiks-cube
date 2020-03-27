;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rubiks-cube-v1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; -- DATA --

;; a Cube is a of (make-cube Face Face Face Face Face Face)
;; and represents the up, down, front, back, right, left faces respectivly
(define-struct cube [u d f b r l])

;; a Face is a (make-face Color Color Color Color Color Color Color Color Color)
;; and represents the stickers on a face in a clockwise spiral starting from UB
(define-struct face [bl b br r fr f fl l c])

;; -- EXAMPLES --

(define white (make-face 'white 'white 'white 'white 'white 'white 'white 'white 'white))
(define yellow (make-face 'yellow 'yellow 'yellow 'yellow 'yellow 'yellow 'yellow 'yellow 'yellow))
(define green (make-face 'green 'green 'green 'green 'green 'green 'green 'green 'green ))
(define blue (make-face 'blue 'blue 'blue 'blue 'blue 'blue 'blue 'blue 'blue ))
(define red (make-face 'red 'red 'red 'red 'red 'red 'red 'red 'red))
(define orange (make-face 'orange 'orange 'orange 'orange 'orange 'orange 'orange 'orange 'orange ))
(define solved (make-cube white yellow green blue red orange))

;; -- TEMPLATES --

;; cube-temp : Cube -> ???
#;(define (cube-temp c)
    (... (cube-u c)
         (cube-d c)
         (cube-f c)
         (cube-b c)
         (cube-r c)
         (cube-l c)))

;; face-temp : Face -> ???
#;(define (face-temp f)
    (... (face-bl f)
         (face-b f)
         (face-br f)
         (face-r f)
         (face-fr f)
         (face-f f)
         (face-fl f)
         (face-l f)
         (face-c f)))

;; -- CONSTANTS --

(define BG (empty-scene 500 500))
(define TILE-SIZE 20)
(define PADDING 5)
(define CUBE-BG (square (+ (* 3 TILE-SIZE) (* 4 PADDING)) 'solid 'black))
(define H-PAD (rectangle TILE-SIZE PADDING 'solid 'black))
(define V-PAD (rotate 90 H-PAD))

;; main : Any -> Cube
;; launches the simluation with a solved cube
(define (main _)
    (big-bang solved
      [to-draw draw]
      [on-key key]))

;; draw : Cube -> Image
;; draws the cube
(define (draw c)
  (overlay (draw-face (cube-f c)) BG))
(check-expect (draw solved) (overlay (draw-face green) BG))

;; draw-face : Face -> Image
(define (draw-face f)
  (overlay (above (beside (draw-tile (face-bl f)) V-PAD (draw-tile (face-b f)) V-PAD (draw-tile (face-br f)))
                  H-PAD
                  (beside (draw-tile (face-l f)) V-PAD (draw-tile (face-c f)) V-PAD (draw-tile (face-r f)))
                  H-PAD
                  (beside (draw-tile (face-fl f)) V-PAD (draw-tile (face-f f)) V-PAD (draw-tile (face-fr f))))
           CUBE-BG))
(check-expect (draw-face white)  (overlay (above (beside (draw-tile 'white) V-PAD (draw-tile 'white) V-PAD (draw-tile 'white))
                                                 H-PAD
                                                 (beside (draw-tile 'white) V-PAD (draw-tile 'white) V-PAD (draw-tile 'white))
                                                 H-PAD
                                                 (beside (draw-tile 'white) V-PAD (draw-tile 'white) V-PAD (draw-tile 'white)))
                                          CUBE-BG))

;; draw-tile : Color -> Image
;; draws the sticker based on the color
(define (draw-tile c)
  (square TILE-SIZE 'solid c))
(check-expect (draw-tile 'white) (square TILE-SIZE 'solid 'white))

;; key : Cube KeyEvent -> Cube
;; processes the user's command
(define (key c ke)
  (cond [(key=? "up" ke) (rotate-up c)]
        [(key=? "down" ke) (rotate-down c)]
        [(key=? "left" ke) (rotate-left c)]
        [(key=? "right" ke) (rotate-right c)]
        [(key=? "u" ke) (u-turn c)]
        [(key=? "i" ke) (ui-turn c)]
        [(key=? "r" ke) (r-turn c)]
        [(key=? "t" ke) (ri-turn c)]
        [(key=? "d" ke) (d-turn c)]
        [(key=? "f" ke) (di-turn c)]
        [(key=? "l" ke) (l-turn c)]
        [(key=? ";" ke) (li-turn c)]
        [else c]))
(check-expect (key solved "up") (rotate-up solved))
(check-expect (key solved "down") (rotate-down solved))
(check-expect (key solved "left") (rotate-left solved))
(check-expect (key solved "right") (rotate-right solved))
(check-expect (key solved "u") (u-turn solved))
(check-expect (key solved "i") (ui-turn solved)) 
(check-expect (key solved "r") (r-turn solved))
(check-expect (key solved "t") (ri-turn solved))
(check-expect (key solved "d") (d-turn solved))
(check-expect (key solved "f") (di-turn solved))
(check-expect (key solved "l") (l-turn solved))
(check-expect (key solved ";") (li-turn solved))
(check-expect (key solved "4") solved)

;; rotate-face-cw : Face -> Face
;; repositions the face clockwise
(define (rotate-face-cw f)
  (make-face (face-fl f)
             (face-l f)
             (face-bl f)
             (face-b f)
             (face-br f)
             (face-r f)
             (face-fr f)
             (face-f f)
             (face-c f)))
(check-expect (rotate-face-cw white) white)

;; rotate-face-ccw : Face -> Face
;; repositions the face counter-clockwise
(define (rotate-face-ccw f)
  (make-face (face-br f)
             (face-r f)
             (face-fr f)
             (face-f f)
             (face-fl f)
             (face-l f)
             (face-bl f)
             (face-b f)
             (face-c f)))
(check-expect (rotate-face-ccw white) white)

;; rotate-up : Cube  -> Cube
;; rotates the entire cube upwards
(define (rotate-up c)
  (make-cube (cube-f c)
             (rotate-face-cw (rotate-face-cw (cube-b c)))
             (cube-d c)
             (rotate-face-cw (rotate-face-cw (cube-u c)))
             (rotate-face-cw (cube-r c))
             (rotate-face-ccw (cube-l c))))
(check-expect (rotate-up solved) (make-cube green blue yellow white (rotate-face-cw red) (rotate-face-ccw orange)))

;; rotate-down : Cube -> Cube
;; rotates the entire cube downwards
(define (rotate-down c)
  (make-cube (rotate-face-cw (rotate-face-cw (cube-b c)))
             (cube-f c)
             (cube-u c)
             (rotate-face-cw (rotate-face-cw (cube-d c)))
             (rotate-face-ccw (cube-r c))
             (rotate-face-cw (cube-l c))))
(check-expect (rotate-down solved) (make-cube blue green white yellow (rotate-face-ccw red) (rotate-face-cw orange)))

;; rotate-left : Cube -> Cube
;; rotates the entire cube to the left
(define (rotate-left c)
  (make-cube (rotate-face-cw (cube-u c))
             (rotate-face-ccw (cube-d c))
             (cube-r c)
             (cube-l c)
             (cube-b c)
             (cube-f c)))
(check-expect (rotate-left solved) (make-cube (rotate-face-cw white) (rotate-face-ccw yellow) red orange blue green))

;; rotate-right : Cube -> Cube
;; rotates the entire cube to the right
(define (rotate-right c)
  (make-cube (rotate-face-ccw (cube-u c))
             (rotate-face-cw (cube-d c))
             (cube-l c)
             (cube-r c)
             (cube-f c)
             (cube-b c)))
(check-expect (rotate-right solved) (make-cube (rotate-face-ccw white) (rotate-face-cw yellow) orange red green blue))

;; rotates the top layer clockwise
(define (u-turn c)
  (local [(define f (cube-f c))
          (define b (cube-b c))
          (define r (cube-r c))
          (define l (cube-l c))]
    (make-cube (rotate-face-cw (cube-u c))
               (cube-d c)
               (make-face (face-bl r) (face-b r) (face-br r)
                          (face-r f) (face-fr f) (face-f f)
                          (face-fl f) (face-l f) (face-c f))
               (make-face (face-bl l) (face-b l) (face-br l)
                          (face-r b) (face-fr b) (face-f b)
                          (face-fl b) (face-l b) (face-c b))
               (make-face (face-bl b) (face-b b) (face-br b)
                          (face-r r) (face-fr r) (face-f r)
                          (face-fl r) (face-l r) (face-c r))
               (make-face (face-bl f) (face-b f) (face-br f)
                          (face-r l) (face-fr l) (face-f l)
                          (face-fl l) (face-l l) (face-c l)))))
(check-expect (u-turn solved) (make-cube white
                                         yellow
                                         (make-face 'red 'red 'red
                                                    'green 'green 'green
                                                    'green 'green 'green)
                                         (make-face 'orange 'orange 'orange
                                                    'blue 'blue 'blue
                                                    'blue 'blue 'blue)
                                         (make-face 'blue 'blue 'blue
                                                    'red 'red 'red
                                                    'red 'red 'red)
                                         (make-face 'green 'green 'green
                                                    'orange 'orange 'orange
                                                    'orange 'orange 'orange)))

;; ui-turn : Cube -> Cube
;; turns the upper layer counter-clockwise
(define (ui-turn c)
  (local [(define f (cube-f c))
          (define b (cube-b c))
          (define r (cube-r c))
          (define l (cube-l c))]
    (make-cube (rotate-face-ccw (cube-u c))
               (cube-d c)
               (make-face (face-bl l) (face-b l) (face-br l)
                          (face-r f) (face-fr f) (face-f f)
                          (face-fl f) (face-l f) (face-c f))
               (make-face (face-bl r) (face-b r) (face-br r)
                          (face-r b) (face-fr b) (face-f b)
                          (face-fl b) (face-l b) (face-c b))
               (make-face (face-bl f) (face-b f) (face-br f)
                          (face-r r) (face-fr r) (face-f r)
                          (face-fl r) (face-l r) (face-c r))
               (make-face (face-bl b) (face-b b) (face-br b)
                          (face-r l) (face-fr l) (face-f l)
                          (face-fl l) (face-l l) (face-c l)))))
(check-expect (ui-turn solved) (make-cube white
                                          yellow
                                          (make-face 'orange 'orange 'orange
                                                     'green 'green 'green
                                                     'green 'green 'green)
                                          (make-face 'red 'red 'red
                                                     'blue 'blue 'blue
                                                     'blue 'blue 'blue)
                                          (make-face 'green 'green 'green
                                                     'red 'red 'red
                                                     'red 'red 'red)
                                          (make-face 'blue 'blue 'blue
                                                     'orange 'orange 'orange
                                                     'orange 'orange 'orange)))

;; r-turn : Cube -> Cube
;; rotates the right layer clockwise
(define (r-turn c)
  (local [(define u (cube-u c))
          (define d (cube-d c))
          (define f (cube-f c))
          (define b (cube-b c))]
    (make-cube (make-face (face-bl u) (face-b u) (face-br f)
                          (face-r f) (face-fr f) (face-f u)
                          (face-fl u) (face-l u) (face-c u))
               (make-face (face-bl d) (face-b d) (face-br b)
                          (face-r b) (face-fr b) (face-f d)
                          (face-fl d) (face-l d) (face-c d))
               (make-face (face-bl f) (face-b f) (face-br d)
                          (face-r d) (face-fr d) (face-f f)
                          (face-fl f) (face-l f) (face-c f))
               (make-face (face-bl u) (face-b b) (face-br b)
                          (face-r b) (face-fr b) (face-f b)
                          (face-fl u) (face-l u) (face-c b))
               (rotate-face-cw (cube-r c))
               (cube-l c))))
(check-expect (r-turn solved) (make-cube (make-face 'white 'white 'green
                                                    'green 'green 'white
                                                    'white 'white 'white)
                                         (make-face 'yellow 'yellow 'blue
                                                    'blue 'blue 'yellow
                                                    'yellow 'yellow 'yellow)
                                         (make-face 'green 'green 'yellow
                                                    'yellow 'yellow 'green
                                                    'green 'green 'green)
                                         (make-face 'white 'blue 'blue
                                                    'blue 'blue 'blue
                                                    'white 'white 'blue)
                                         red orange))

;; ri-turn : Cube -> Cube
;; turns the right layer counter-clockwise
(define (ri-turn c)
  (local [(define u (cube-u c))
          (define d (cube-d c))
          (define f (cube-f c))
          (define b (cube-b c))]
    (make-cube (make-face (face-bl u) (face-b u) (face-br b)
                          (face-r b) (face-fr b) (face-f u)
                          (face-fl u) (face-l u) (face-c u))
               (make-face (face-bl d) (face-b d) (face-br f)
                          (face-r f) (face-fr f) (face-f d)
                          (face-fl d) (face-l d) (face-c d))
               (make-face (face-bl f) (face-b f) (face-br u)
                          (face-r u) (face-fr u) (face-f f)
                          (face-fl f) (face-l f) (face-c f))
               (make-face (face-bl d) (face-b b) (face-br b)
                          (face-r b) (face-fr b) (face-f b)
                          (face-fl d) (face-l d) (face-c b))
               (rotate-face-ccw (cube-r c))
               (cube-l c))))
(check-expect (ri-turn solved) (make-cube (make-face 'white 'white 'blue
                                                    'blue 'blue 'white
                                                    'white 'white 'white)
                                         (make-face 'yellow 'yellow 'green
                                                    'green 'green 'yellow
                                                    'yellow 'yellow 'yellow)
                                         (make-face 'green 'green 'white
                                                    'white 'white 'green
                                                    'green 'green 'green)
                                         (make-face 'yellow 'blue 'blue
                                                    'blue 'blue 'blue
                                                    'yellow 'yellow 'blue)
                                         red orange))

;; d-turn : Cube -> Cube
;; rotates the bottom layer clockwise
(define (d-turn c)
  (local [(define f (cube-f c))
          (define b (cube-b c))
          (define r (cube-r c))
          (define l (cube-l c))]
    (make-cube (cube-u c)
               (rotate-face-cw (cube-d c))
               (make-face (face-bl f) (face-b f) (face-br f)
                          (face-r f) (face-fr l) (face-f l)
                          (face-fl l) (face-l f) (face-c f))
               (make-face (face-bl b) (face-b b) (face-br b)
                          (face-r b) (face-fr r) (face-f r)
                          (face-fl r) (face-l b) (face-c b))
               (make-face (face-bl r) (face-b r) (face-br r)
                          (face-r r) (face-fr f) (face-f f)
                          (face-fl f) (face-l r) (face-c r))
               (make-face (face-bl l) (face-b l) (face-br l)
                          (face-r l) (face-fr b) (face-f b)
                          (face-fl b) (face-l l) (face-c l)))))
(check-expect (d-turn solved) (make-cube white
                                         yellow
                                         (make-face 'green 'green 'green
                                                    'green 'orange 'orange
                                                    'orange 'green 'green)
                                         (make-face 'blue 'blue 'blue
                                                    'blue 'red 'red
                                                    'red 'blue 'blue)
                                         (make-face 'red 'red 'red
                                                    'red 'green 'green
                                                    'green 'red 'red)
                                         (make-face 'orange 'orange 'orange
                                                    'orange 'blue 'blue
                                                    'blue 'orange 'orange)))

;; di-turn : Cube -> Cube
;; rotates the bototm layer counter-clockwise
(define (di-turn c)
  (local [(define f (cube-f c))
          (define b (cube-b c))
          (define r (cube-r c))
          (define l (cube-l c))]
    (make-cube (cube-u c)
               (rotate-face-ccw (cube-d c))
               (make-face (face-bl f) (face-b f) (face-br f)
                          (face-r f) (face-fr r) (face-f r)
                          (face-fl r) (face-l f) (face-c f))
               (make-face (face-bl b) (face-b b) (face-br b)
                          (face-r b) (face-fr l) (face-f l)
                          (face-fl l) (face-l b) (face-c b))
               (make-face (face-bl r) (face-b r) (face-br r)
                          (face-r r) (face-fr b) (face-f b)
                          (face-fl b) (face-l r) (face-c r))
               (make-face (face-bl l) (face-b l) (face-br l)
                          (face-r l) (face-fr f) (face-f f)
                          (face-fl f) (face-l l) (face-c l)))))
(check-expect (di-turn solved) (make-cube white
                                          yellow
                                          (make-face 'green 'green 'green
                                                     'green 'red 'red
                                                     'red 'green 'green)
                                          (make-face 'blue 'blue 'blue
                                                     'blue 'orange 'orange
                                                     'orange 'blue 'blue)
                                          (make-face 'red 'red 'red
                                                     'red 'blue 'blue
                                                     'blue 'red 'red)
                                          (make-face 'orange 'orange 'orange
                                                     'orange 'green 'green
                                                     'green 'orange 'orange)))

;; l-turn : Cube -> Cube
;; rotates the left layer clockwise
(define (l-turn c)
  (local [(define u (cube-u c))
          (define d (cube-d c))
          (define f (cube-f c))
          (define b (cube-b c))]
    (make-cube (make-face (face-bl b) (face-b u) (face-br u)
                          (face-r u) (face-fr u) (face-f u)
                          (face-fl b) (face-l b) (face-c u))
               (make-face (face-bl f) (face-b d) (face-br d)
                          (face-r d) (face-fr d) (face-f d)
                          (face-fl f) (face-l f) (face-c d))
               (make-face (face-bl u) (face-b f) (face-br f)
                          (face-r f) (face-fr f) (face-f f)
                          (face-fl u) (face-l u) (face-c f))
               (make-face (face-bl b) (face-b b) (face-br d)
                          (face-r d) (face-fr d) (face-f b)
                          (face-fl b) (face-l b) (face-c b))
               (cube-r c) (rotate-face-cw (cube-l c)))))
(check-expect (l-turn solved) (make-cube (make-face 'blue 'white 'white
                                                    'white 'white 'white
                                                    'blue 'blue 'white)
                                         (make-face 'green 'yellow 'yellow
                                                    'yellow 'yellow 'yellow
                                                    'green 'green 'yellow)
                                         (make-face 'white 'green 'green
                                                    'green 'green 'green
                                                    'white 'white 'green)
                                         (make-face 'blue 'blue 'yellow
                                                    'yellow 'yellow 'blue
                                                    'blue 'blue 'blue)
                                         red orange))

;; li-turn : Cube -> Cube
;; rotates the left layer counter-clockwise
(define (li-turn c)
  (local [(define u (cube-u c))
          (define d (cube-d c))
          (define f (cube-f c))
          (define b (cube-b c))]
    (make-cube (make-face (face-bl f) (face-b u) (face-br u)
                          (face-r u) (face-fr u) (face-f u)
                          (face-fl f) (face-l f) (face-c u))
               (make-face (face-bl b) (face-b d) (face-br d)
                          (face-r d) (face-fr d) (face-f d)
                          (face-fl b) (face-l b) (face-c d))
               (make-face (face-bl d) (face-b f) (face-br f)
                          (face-r f) (face-fr f) (face-f f)
                          (face-fl d) (face-l d) (face-c f))
               (make-face (face-bl b) (face-b b) (face-br u)
                          (face-r u) (face-fr u) (face-f b)
                          (face-fl b) (face-l b) (face-c b))
               (cube-r c) (rotate-face-ccw (cube-l c)))))

(check-expect (li-turn solved) (make-cube (make-face 'green 'white 'white
                                                     'white 'white 'white
                                                     'green 'green 'white)
                                          (make-face 'blue 'yellow 'yellow
                                                     'yellow 'yellow 'yellow
                                                     'blue 'blue 'yellow)
                                          (make-face 'yellow 'green 'green
                                                     'green 'green 'green
                                                     'yellow 'yellow 'green)
                                          (make-face 'blue 'blue 'white
                                                     'white 'white 'blue
                                                     'blue 'blue 'blue)
                                          red orange))