;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rubiks-cube-v2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; -- DATA --

#| a Cube is a (make-cube Color Color Color Color Color Color Color Color Color
                          Color Color Color Color Color Color Color Color Color
                          Color Color Color Color Color Color Color Color Color
                          Color Color Color Color Color Color Color Color Color
                          Color Color Color Color Color Color Color Color Color
                          Color Color Color Color Color Color Color Color Color) |#
;; and represents all 56 stickers of a cube in a specific scheme

(define-struct cube [ubl ub ubr ul u ur ufl uf ufr
                         lbu lu lfu lb l lf ldb ld ldf
                         ful fu fur fl f fr fdl fd frd
                         ruf ru rub rf r rb rdf rd rdb
                         bur bu bul br b bl bdr bd bdl
                         dfl df dfr dl d dr dbl db dbr])

;; -- EXAMPLE --

(define solved (make-cube 'white 'white 'white 'white 'white 'white 'white 'white 'white
                          'orange 'orange 'orange 'orange 'orange 'orange 'orange 'orange 'orange
                          'green 'green 'green 'green 'green 'green 'green 'green 'green
                          'red 'red 'red 'red 'red 'red 'red 'red 'red
                          'blue 'blue 'blue 'blue 'blue 'blue 'blue 'blue 'blue
                          'yellow 'yellow 'yellow 'yellow 'yellow 'yellow 'yellow 'yellow 'yellow))

;; -- TEMPLATE --

;; cube-temp : Cube -> ???
#;(define (cube-temp c)
    (... (cube-ubl c) (cube-ub c) (cube-ubr c) (cube-ul c) (cube-u c) (cube-ur c) (cube-ufl c) (cube-uf c) (cube-ufr c)
         (cube-lbu c) (cube-lu c) (cube-lfu c) (cube-lb c) (cube-l c) (cube-lf c) (cube-ldb c) (cube-ld c) (cube-ldf c)
         (cube-ful c) (cube-fu c) (cube-fur c) (cube-fl c) (cube-f c) (cube-fr c) (cube-fdl c) (cube-fd c) (cube-frd c)
         (cube-ruf c) (cube-ru c) (cube-rub c) (cube-rf c) (cube-r c) (cube-rb c) (cube-rdf c) (cube-rd c) (cube-rdb c)
         (cube-bur c) (cube-bu c) (cube-bul c) (cube-br c) (cube-b c) (cube-bl c) (cube-bdr c) (cube-bd c) (cube-bdl c)
         (cube-dfl c) (cube-df c) (cube-dfr c) (cube-dl c) (cube-d c) (cube-dr c) (cube-dbl c) (cube-db c) (cube-dbr c)))

;; -- CONSTANTS --

(define BG-SIZE 500)
(define BG (empty-scene BG-SIZE BG-SIZE))
(define TILE-SIZE 40)
(define PADDING (/ TILE-SIZE 4))
(define CUBE-BG (square (+ (* 3 TILE-SIZE) (* 4 PADDING)) 'solid 'black))
(define H-PAD (rectangle TILE-SIZE PADDING 'solid 'black))
(define V-PAD (rotate 90 H-PAD))
(define H-PANEL (rectangle (image-height CUBE-BG) (+ PADDING TILE-SIZE) 'solid 'black))
(define V-PANEL (rotate 90 H-PANEL))
(define SOLVE-BUTTON (underlay (rectangle (* 3 TILE-SIZE) TILE-SIZE 'solid (make-color 250 100 5))
                               (text "Solve" (/ TILE-SIZE 2) 'white)))
(define SCRAMBLE-BUTTON (underlay (rectangle (* 3 TILE-SIZE) TILE-SIZE 'solid (make-color 250 100 5))
                                 (text "Scramble" (/ TILE-SIZE 2) 'white)))

;; -- LAUNCH --

;; main : Any -> Cube
;; launches main sim with a a solved cube
(define (main _)
  (big-bang solved
    [to-draw draw]
    [on-key key]
    [on-mouse mouse]))

;; -- DRAW --

;; draw : Cube -> Image
;; draws the 2D net of the cube onto the background
(define (draw c)
  (place-image/align SCRAMBLE-BUTTON 25 (- BG-SIZE 25) 'left 'bottom
                     (place-image/align SOLVE-BUTTON (- BG-SIZE 25) (- BG-SIZE 25) 'right 'bottom (overlay (net c) BG))))
(check-expect (draw solved) (place-image/align SCRAMBLE-BUTTON
                                               25 (- BG-SIZE 25)
                                               'left 'bottom
                                               (place-image/align SOLVE-BUTTON
                                                                  (- BG-SIZE 25) (- BG-SIZE 25) 'right 'bottom
                                                                  (overlay (net solved) BG))))

;; net : Cube -> Image
;; draws the net
(define (net c)
  (above (make-horizontal-row (cube-ufl c) (cube-uf c) (cube-ufr c) 'bottom)
         (beside (make-vertical-row (cube-lfu c) (cube-lf c) (cube-ldf c) 'right)
                 (draw-face (cube-ful c) (cube-fu c) (cube-fur c)
                            (cube-fl c) (cube-f c) (cube-fr c)
                            (cube-fdl c) (cube-fd c) (cube-frd c))
                 (make-vertical-row (cube-ruf c) (cube-rf c) (cube-rdf c) 'left))
         (make-horizontal-row (cube-dfl c) (cube-df c) (cube-dfr c) 'top)))
(check-expect (net solved) (above (make-horizontal-row 'white 'white 'white 'bottom)
                                  (beside (make-vertical-row 'orange 'orange 'orange 'right)
                                          (draw-face 'green 'green 'green
                                                     'green 'green 'green
                                                     'green 'green 'green)
                                          (make-vertical-row 'red 'red 'red 'left))
                                  (make-horizontal-row 'yellow 'yellow 'yellow 'top)))

;; make-horizontal-row : Color Color Color Symbol -> Image
;; draws a horizontal row given the position
(define (make-horizontal-row c1 c2 c3 loc)
  (overlay/align 'middle loc (beside (make-tile c1) V-PAD (make-tile c2) V-PAD (make-tile c3))
                 H-PANEL))
(check-expect (make-horizontal-row 'red 'green 'blue 'bottom) (overlay/align 'middle 'bottom (beside (make-tile 'red)
                                                                                                     V-PAD
                                                                                                     (make-tile 'green)
                                                                                                     V-PAD
                                                                                                     (make-tile 'blue))
                                                                             H-PANEL))

;; make-vertical-row : Color Color Color Symbol -> Image
;; draws the vertical row given a position
(define (make-vertical-row c1 c2 c3 loc)
  (overlay/align loc 'middle (above (make-tile c1)
                                    H-PAD
                                    (make-tile c2)
                                    H-PAD
                                    (make-tile c3))
                 V-PANEL))
(check-expect (make-vertical-row 'red 'green 'blue 'left) (overlay/align 'left 'middle (above (make-tile 'red)
                                                                                              H-PAD
                                                                                              (make-tile 'green)
                                                                                              H-PAD
                                                                                              (make-tile 'blue))
                                                                         V-PANEL))

;; draw-face : Color Color Color Color Color Color Color Color Color -> Image
;; draws the face of the cube
(define (draw-face c1 c2 c3 c4 c5 c6 c7 c8 c9)
  (overlay (above (beside (make-tile c1) V-PAD (make-tile c2) V-PAD (make-tile c3))
                  H-PAD
                  (beside (make-tile c4) V-PAD (make-tile c5) V-PAD (make-tile c6))
                  H-PAD
                  (beside (make-tile c7) V-PAD (make-tile c8) V-PAD (make-tile c9)))
           CUBE-BG))
(check-expect (draw-face 'red 'green 'blue 'red 'green 'blue 'red 'green 'blue)
              (overlay (above (beside (make-tile 'red) V-PAD (make-tile 'green) V-PAD (make-tile 'blue))
                              H-PAD
                              (beside (make-tile 'red) V-PAD (make-tile 'green) V-PAD (make-tile 'blue))
                              H-PAD
                              (beside (make-tile 'red) V-PAD (make-tile 'green) V-PAD (make-tile 'blue)))
                       CUBE-BG))

;; make-tile : Color -> Image
;; draws the sticker
(define (make-tile c)
  (square TILE-SIZE 'solid c))
(check-expect (make-tile 'red) (square TILE-SIZE 'solid 'red))

;; -- KEY --

;; three : [X -> X] -> [X -> X]
;; does a move three times
(define (three f)
  (λ (x)
    (f (f (f x)))))
(check-expect ((three add1) 0) 3)

;; key : Cube KeyEvent -> Cube
;; processes the keystrokes
(define (key c ke)
  (cond [(key=? "up" ke) (rotate-up c)]
        [(key=? "down" ke) ((three rotate-up) c)]
        [(key=? "left" ke) (rotate-left c)]
        [(key=? "right" ke) ((three rotate-left) c)]
        [(key=? "p" ke) (z-rotate c)]
        [(key=? "q" ke) ((three z-rotate) c)]
        [(key=? "i" ke) (r-turn c)]
        [(key=? "k" ke) ((three r-turn) c)]
        [(key=? "d" ke) (l-turn c)]
        [(key=? "e" ke) ((three l-turn) c)]
        [(key=? "j" ke) (u-turn c)]
        [(key=? "f" ke) ((three u-turn) c)]
        [(key=? "s" ke) (d-turn c)]
        [(key=? "l" ke) ((three d-turn) c)]
        [(key=? "h" ke) (f-turn c)]
        [(key=? "g" ke) ((three f-turn) c)]
        [else c]))
(check-expect (key solved "up") (rotate-up solved))
(check-expect (key solved "down") ((three rotate-up) solved))
(check-expect (key solved "left") (rotate-left solved))
(check-expect (key solved "right") ((three rotate-left) solved))
(check-expect (key solved "p") (z-rotate solved))
(check-expect (key solved "q") ((three z-rotate) solved))
(check-expect (key solved "i") (r-turn solved)) 
(check-expect (key solved "k") ((three r-turn) solved))
(check-expect (key solved "d") (l-turn solved))
(check-expect (key solved "e") ((three l-turn) solved))
(check-expect (key solved "j") (u-turn solved))
(check-expect (key solved "f") ((three u-turn) solved))
(check-expect (key solved "s") (d-turn solved))
(check-expect (key solved "l") ((three d-turn) solved))
(check-expect (key solved "h") (f-turn solved))
(check-expect (key solved "g") ((three f-turn) solved))
(check-expect (key solved "5") solved)

;; rotate-up : Cube -> Cube
;; simulates pushing all three vertical layer up
(define (rotate-up c)
  (make-cube (cube-ful c) (cube-fu c) (cube-fur c) (cube-fl c) (cube-f c) (cube-fr c) (cube-fdl c) (cube-fd c) (cube-frd c)
             (cube-lfu c) (cube-lf c) (cube-ldf c) (cube-lu c) (cube-l c) (cube-ld c) (cube-lbu c) (cube-lb c) (cube-ldb c)
             (cube-dfl c) (cube-df c) (cube-dfr c) (cube-dl c) (cube-d c) (cube-dr c) (cube-dbl c) (cube-db c) (cube-dbr c)
             (cube-rdf c) (cube-rf c) (cube-ruf c) (cube-rd c) (cube-r c) (cube-ru c) (cube-rdb c) (cube-rb c) (cube-rub c)
             (cube-ufr c) (cube-uf c) (cube-ufl c) (cube-ur c) (cube-u c) (cube-ul c) (cube-ubr c) (cube-ub c) (cube-ubl c)
             (cube-bdl c) (cube-bd c) (cube-bdr c) (cube-bl c) (cube-b c) (cube-br c) (cube-bul c) (cube-bu c) (cube-bur c)))
(check-expect (rotate-up solved) (make-cube 'green 'green 'green 'green 'green 'green 'green 'green 'green
                                            'orange 'orange 'orange 'orange 'orange 'orange 'orange 'orange 'orange
                                            'yellow 'yellow 'yellow 'yellow 'yellow 'yellow 'yellow 'yellow 'yellow
                                            'red 'red 'red 'red 'red 'red 'red 'red 'red
                                            'white 'white 'white 'white 'white 'white 'white 'white 'white
                                            'blue 'blue 'blue 'blue 'blue 'blue 'blue 'blue 'blue))
                                            

;; rotate-left : Cube -> Cube
;; simulates pushing the entire cube to the left
(define (rotate-left c)
  (make-cube (cube-ufl c) (cube-ul c) (cube-ubl c) (cube-uf c) (cube-u c) (cube-ub c) (cube-ufr c) (cube-ur c) (cube-ubr c)
             (cube-ful c) (cube-fu c) (cube-fur c) (cube-fl c) (cube-f c) (cube-fr c) (cube-fdl c) (cube-fd c) (cube-frd c)
             (cube-ruf c) (cube-ru c) (cube-rub c) (cube-rf c) (cube-r c) (cube-rb c) (cube-rdf c) (cube-rd c) (cube-rdb c)
             (cube-bur c) (cube-bu c) (cube-bul c) (cube-br c) (cube-b c) (cube-bl c) (cube-bdr c) (cube-bd c) (cube-bdl c)
             (cube-lbu c) (cube-lu c) (cube-lfu c) (cube-lb c) (cube-l c) (cube-lf c) (cube-ldb c) (cube-ld c) (cube-ldf c)
             (cube-dfr c) (cube-dr c) (cube-dbr c) (cube-df c) (cube-d c) (cube-db c) (cube-dfl c) (cube-dl c) (cube-dbl c)))
(check-expect (rotate-left solved) (make-cube 'white 'white 'white 'white 'white 'white 'white 'white 'white
                                              'green 'green 'green 'green 'green 'green 'green 'green 'green
                                              'red 'red 'red 'red 'red 'red 'red 'red 'red
                                              'blue 'blue 'blue 'blue 'blue 'blue 'blue 'blue 'blue
                                              'orange 'orange 'orange 'orange 'orange 'orange 'orange 'orange 'orange
                                              'yellow 'yellow 'yellow 'yellow 'yellow 'yellow 'yellow 'yellow 'yellow))
 
;; z-rotate : Cube -> Cube
;; rotates the cube clockwise on the z axis
(define (z-rotate c)
  (make-cube (cube-ldb c) (cube-lb c) (cube-lbu c) (cube-ld c) (cube-l c) (cube-lu c) (cube-ldf c) (cube-lf c) (cube-lfu c)
             (cube-dbl c) (cube-dl c) (cube-dfl c) (cube-db c) (cube-d c) (cube-df c) (cube-dbr c) (cube-dr c) (cube-dfr c)
             (cube-fdl c) (cube-fl c) (cube-ful c) (cube-fd c) (cube-f c) (cube-fu c) (cube-frd c) (cube-fr c) (cube-fur c)
             (cube-ufl c) (cube-ul c) (cube-ubl c) (cube-uf c) (cube-u c) (cube-ub c) (cube-ufr c) (cube-ur c) (cube-ubr c)
             (cube-bul c) (cube-bl c) (cube-bdl c) (cube-bu c) (cube-b c) (cube-bd c) (cube-bur c) (cube-br c) (cube-bdr c)
             (cube-rdf c) (cube-rf c) (cube-ruf c) (cube-rd c) (cube-r c) (cube-ru c) (cube-rdb c) (cube-rb c) (cube-rub c)))
(check-expect (z-rotate solved) (make-cube 'orange 'orange 'orange 'orange 'orange 'orange 'orange 'orange 'orange
                                           'yellow 'yellow 'yellow 'yellow 'yellow 'yellow 'yellow 'yellow 'yellow
                                           'green 'green 'green 'green 'green 'green 'green 'green 'green
                                           'white 'white 'white 'white 'white 'white 'white 'white 'white
                                           'blue 'blue 'blue 'blue 'blue 'blue 'blue 'blue 'blue
                                           'red 'red 'red 'red 'red 'red 'red 'red 'red))

;; r-turn : Cube -> Cube
;; simulates moving the right layer clockwise
(define (r-turn c)
  (make-cube (cube-ubl c) (cube-ub c) (cube-fur c) (cube-ul c) (cube-u c) (cube-fr c) (cube-ufl c) (cube-uf c) (cube-frd c)
             (cube-lbu c) (cube-lu c) (cube-lfu c) (cube-lb c) (cube-l c) (cube-lf c) (cube-ldb c) (cube-ld c) (cube-ldf c)
             (cube-ful c) (cube-fu c) (cube-dfr c) (cube-fl c) (cube-f c) (cube-dr c) (cube-fdl c) (cube-fd c) (cube-dbr c)
             (cube-rdf c) (cube-rf c) (cube-ruf c) (cube-rd c) (cube-r c) (cube-ru c) (cube-rdb c) (cube-rb c) (cube-rub c)
             (cube-ufr c) (cube-bu c) (cube-bul c) (cube-ur c) (cube-b c) (cube-bl c) (cube-ubr c) (cube-bd c) (cube-bdl c)
             (cube-dfl c) (cube-df c) (cube-bdr c) (cube-dl c) (cube-d c) (cube-br c) (cube-dbl c) (cube-db c) (cube-bur c)))
(check-expect (r-turn solved) (make-cube 'white 'white 'green 'white 'white 'green 'white 'white 'green
                                         'orange 'orange 'orange 'orange 'orange 'orange 'orange 'orange 'orange
                                         'green 'green 'yellow 'green 'green 'yellow 'green 'green 'yellow
                                         'red 'red 'red 'red 'red 'red 'red 'red 'red
                                         'white 'blue 'blue 'white 'blue 'blue 'white 'blue 'blue
                                         'yellow 'yellow 'blue 'yellow 'yellow 'blue 'yellow 'yellow 'blue))

;; l-turn : Cube -> Cube
;; simulates moving the left layer clockwise
(define (l-turn c)
  (make-cube (cube-bdl c) (cube-ub c) (cube-ubr c) (cube-bl c) (cube-u c) (cube-ur c) (cube-bul c) (cube-uf c) (cube-ufr c)
             (cube-ldb c) (cube-lb c) (cube-lbu c) (cube-ld c) (cube-l c) (cube-lu c) (cube-ldf c) (cube-lf c) (cube-lfu c)
             (cube-ubl c) (cube-fu c) (cube-fur c) (cube-ul c) (cube-f c) (cube-fr c) (cube-ufl c) (cube-fd c) (cube-frd c)
             (cube-ruf c) (cube-ru c) (cube-rub c) (cube-rf c) (cube-r c) (cube-rb c) (cube-rdf c) (cube-rd c) (cube-rdb c)
             (cube-bur c) (cube-bu c) (cube-dbl c) (cube-br c) (cube-b c) (cube-dl c) (cube-bdr c) (cube-bd c) (cube-dfl c)
             (cube-ful c) (cube-df c) (cube-dfr c) (cube-fl c) (cube-d c) (cube-dr c) (cube-fdl c) (cube-db c) (cube-dbr c)))
(check-expect (l-turn solved) (make-cube 'blue 'white 'white 'blue 'white 'white 'blue 'white 'white
                                         'orange 'orange 'orange 'orange 'orange 'orange 'orange 'orange 'orange
                                         'white 'green 'green 'white 'green 'green 'white 'green 'green
                                         'red 'red 'red 'red 'red 'red 'red 'red 'red
                                         'blue 'blue 'yellow 'blue 'blue 'yellow 'blue 'blue 'yellow
                                         'green 'yellow 'yellow 'green 'yellow 'yellow 'green 'yellow 'yellow))

;; u-turn : Cube -> Cube
;; simulates moving the top layer clockwise
(define (u-turn c)
  (make-cube (cube-ufl c) (cube-ul c) (cube-ubl c) (cube-uf c) (cube-u c) (cube-ub c) (cube-ufr c) (cube-ur c) (cube-ubr c)
             (cube-ful c) (cube-fu c) (cube-fur c) (cube-lb c) (cube-l c) (cube-lf c) (cube-ldb c) (cube-ld c) (cube-ldf c)
             (cube-ruf c) (cube-ru c) (cube-rub c) (cube-fl c) (cube-f c) (cube-fr c) (cube-fdl c) (cube-fd c) (cube-frd c)
             (cube-bur c) (cube-bu c) (cube-bul c) (cube-rf c) (cube-r c) (cube-rb c) (cube-rdf c) (cube-rd c) (cube-rdb c)
             (cube-lbu c) (cube-lu c) (cube-lfu c) (cube-br c) (cube-b c) (cube-bl c) (cube-bdr c) (cube-bd c) (cube-bdl c)
             (cube-dfl c) (cube-df c) (cube-dfr c) (cube-dl c) (cube-d c) (cube-dr c) (cube-dbl c) (cube-db c) (cube-dbr c)))
(check-expect (u-turn solved) (make-cube 'white 'white 'white 'white 'white 'white 'white 'white 'white
                                         'green 'green 'green 'orange 'orange 'orange 'orange 'orange 'orange
                                         'red 'red 'red 'green 'green 'green 'green 'green 'green
                                         'blue 'blue 'blue 'red 'red 'red 'red 'red 'red
                                         'orange 'orange 'orange 'blue 'blue 'blue 'blue 'blue 'blue
                                         'yellow 'yellow 'yellow 'yellow 'yellow 'yellow 'yellow 'yellow 'yellow))

;; d-turn : Cube -> Cube
;; simulates moving the bottom layer clockwise
(define (d-turn c)
  (make-cube (cube-ubl c) (cube-ub c) (cube-ubr c) (cube-ul c) (cube-u c) (cube-ur c) (cube-ufl c) (cube-uf c) (cube-ufr c)
             (cube-lbu c) (cube-lu c) (cube-lfu c) (cube-lb c) (cube-l c) (cube-lf c) (cube-bdr c) (cube-bd c) (cube-bdl c)
             (cube-ful c) (cube-fu c) (cube-fur c) (cube-fl c) (cube-f c) (cube-fr c) (cube-ldb c) (cube-ld c) (cube-ldf c)
             (cube-ruf c) (cube-ru c) (cube-rub c) (cube-rf c) (cube-r c) (cube-rb c) (cube-fdl c) (cube-fd c) (cube-frd c)
             (cube-bur c) (cube-bu c) (cube-bul c) (cube-br c) (cube-b c) (cube-bl c) (cube-rdf c) (cube-rd c) (cube-rdb c)
             (cube-dbl c) (cube-dl c) (cube-dfl c) (cube-db c) (cube-d c) (cube-df c) (cube-dbr c) (cube-dr c) (cube-dfr c)))
(check-expect (d-turn solved) (make-cube 'white 'white 'white 'white 'white 'white 'white 'white 'white
                                         'orange 'orange 'orange 'orange 'orange 'orange 'blue 'blue 'blue
                                         'green 'green 'green 'green 'green 'green 'orange 'orange 'orange
                                         'red 'red 'red 'red 'red 'red 'green 'green 'green
                                         'blue 'blue 'blue 'blue 'blue 'blue 'red 'red 'red
                                         'yellow 'yellow 'yellow 'yellow 'yellow 'yellow 'yellow 'yellow 'yellow))

;; f-turn : Cube -> Cube
;; simulates moving the front layer clockwise 
(define (f-turn c)
  (make-cube (cube-ubl c) (cube-ub c) (cube-ubr c) (cube-ul c) (cube-u c) (cube-ur c) (cube-ldf c) (cube-lf c) (cube-lfu c)
             (cube-lbu c) (cube-lu c) (cube-dfl c) (cube-lb c) (cube-l c) (cube-df c) (cube-ldb c) (cube-ld c) (cube-dfr c)
             (cube-fdl c) (cube-fl c) (cube-ful c) (cube-fd c) (cube-f c) (cube-fu c) (cube-frd c) (cube-fr c) (cube-fur c)
             (cube-ufl c) (cube-ru c) (cube-rub c) (cube-uf c) (cube-r c) (cube-rb c) (cube-ufr c) (cube-rd c) (cube-rdb c)
             (cube-bur c) (cube-bu c) (cube-bul c) (cube-br c) (cube-b c) (cube-bl c) (cube-bdr c) (cube-bd c) (cube-bdl c)
             (cube-rdf c) (cube-rf c) (cube-ruf c) (cube-dl c) (cube-d c) (cube-dr c) (cube-dbl c) (cube-db c) (cube-dbr c)))
(check-expect (f-turn solved) (make-cube 'white 'white 'white 'white 'white 'white 'orange 'orange 'orange
                                         'orange 'orange 'yellow 'orange 'orange 'yellow 'orange 'orange 'yellow
                                         'green 'green 'green 'green 'green 'green 'green 'green 'green
                                         'white 'red 'red 'white 'red 'red 'white 'red 'red
                                         'blue 'blue 'blue 'blue 'blue 'blue 'blue 'blue 'blue
                                         'red 'red 'red 'yellow 'yellow 'yellow 'yellow 'yellow 'yellow))

;; -- MOUSE --

;; mouse : Cube Nat Nat MouseEvent -> Cube
;; processes the users mouse input
(define (mouse c x y me)
  (cond [(and (string=? "button-down" me) (on-button/solve? x y)) solved]
        [(and (string=? "button-down" me) (on-button/scramble? x y)) (random-scramble 0)]
        [else c]))
(check-expect (mouse solved 474 474 "button-down") solved)
(check-satisfied (mouse solved 26 474 "button-down") cube?)
(check-expect (mouse solved 43 34 "drag") solved)

;; on-button/solve? : Nat Nat -> Boolean
;; was the click on the solve button
(define (on-button/solve? x y)
  (and (<= 355 x 475)
       (<= 435 y 475)))
(check-expect (on-button/solve? 360 450) #t)
(check-expect (on-button/solve? 360 1) #f)
(check-expect (on-button/solve? 1 450) #f)
(check-expect (on-button/solve? 1 1) #f)

;; on-button/scramble? : Nat Nat -> Boolean
;; was the click on the scramble button
(define (on-button/scramble? x y)
  (and (<= 25 x 145)
       (<= 435 y 475)))
(check-expect (on-button/scramble? 30 440) #t)
(check-expect (on-button/scramble? 30 1) #f)
(check-expect (on-button/scramble? 200 440) #f)
(check-expect (on-button/scramble? 1 1) #f)

(define MOVES (list rotate-up (three rotate-up) rotate-left (three rotate-left) z-rotate (three z-rotate)
                    r-turn (three r-turn) l-turn (three l-turn) u-turn (three u-turn) d-turn (three d-turn)
                    f-turn (three f-turn)))

;; random-scramble : Any -> Cube
;; generates a random scramble
(define (random-scramble _)
  (foldr (λ (move cube) (move cube)) solved (map (λ (n) (list-ref MOVES n)) (make-random-seed 150))))
(check-satisfied (random-scramble 0) cube?)
 
;; make-random-seed : Nat -> [list-of Nat]
;; creates a list of numbers bound by number of moves with length n
(define (make-random-seed n)
  (cond [(zero? n) empty]
        [(positive? n) (cons (random (length MOVES)) (make-random-seed (sub1 n)))]))
(check-satisfied (make-random-seed 40) (λ (lon) (= (length lon) 40)))