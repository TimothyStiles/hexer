(ns hexer.core
  (:gen-class)
  (:require [thi.ng.geom.core :as g]
            [thi.ng.geom.polygon :as p]
            [thi.ng.math.core :as m]
            [thi.ng.geom.gmesh :as gm]
            [thi.ng.geom.mesh.io :as mio]
            [clojure.java.io :as io]))

(defn save-stl
  "Takes file path and mesh instance, saves mesh as STL."
  [path mesh]
  (with-open [out (io/output-stream path)]
    (mio/write-stl
      (mio/wrapped-output-stream out)
      (g/tessellate mesh))))

(defn point-shift
  [point]
  (if (odd? (first point))
    [(first point) (- (second point) (/ 1 2))]
    [(- (first point) (/ 1 2)) (+ (second point) (/ 1 4))]))

(defn center-points
  "eventually include x-range y-range."
  ([] (center-points 6))
  ([n] 
   (let [xn (int (Math/ceil (/ n 2)))
         x1 (map #(* (/ 3 2) %) (range 0 xn))
         x2 (map #(+ (* (/ 3 2) %) (/ 3 4)) (range 0 xn))
         y1 (map #(* % (/ (Math/sqrt 3) 2)) (range 0 n))
         y2 (map #(- (* % (/ (Math/sqrt 3) 2)) (/ (Math/sqrt 3) 4)) (range 0 n))
         whole-x (for [y y1 x x1] [x y])
         whole-y (for [y y2 x x2] [x y])]
     (partition 2 (flatten [whole-x whole-y])))))


(defn ring-filter
  [radius vec]
  (and (zero? (reduce + vec)) ;(= radius (reduce #(+ (Math/abs %1) (Math/abs %2)) vec))
       ))

(defn ring
  [radius]
  (filter #(ring-filter radius %)
          (for [dx (range (- radius) (inc radius))
                dy (range (- radius) (inc radius))
                dz (range (- radius) (inc radius))]
            [dx dy dz])))

(defn round 
  [radius]
  (mapv vec (partition 3 (flatten (map #(ring %) (range radius))))))

(defn flat-cube->pixel
  [vec]
  [(* (first vec) (/ 3 4)) (* (+ (/ (first vec) 2) (second vec)) (/ (Math/sqrt 3) 2))])

(defn hex-corner
  "Given a size and number returns corresponding hex edge.
  Translated from: http://www.redblobgames.com/grids/hexagons/"
  [size number]
  (let [angle-deg (* 60 number)
        angle-rad (* (/ m/PI 180) angle-deg)
        x (* size (Math/cos angle-rad))
        y (* size (Math/sin angle-rad))]
    [x y]))

(defn hex
  "eventually include t as scalar."
  ([] (hex (/ 1 2)))
  ([t]
   (p/polygon2 (mapv #(hex-corner t  %) (range 6)))))

(defn honey-comb-grid
  []
  (map #(g/extrude % {:depth (rand-nth (range 1 5)) :mesh (gm/gmesh)}) (map #(g/center (hex) %) (center-points 6))))

(defn hex-grid
  []
  (let [points (map flat-cube->pixel (ring 10)) 
        hexagons (map #(g/center (hex) %) points)
        grad (map #(+ 0.2 (Math/cos (first %)) (Math/cos (second %))) points)
        gradient (map #(+ (Math/abs (apply min grad)) %) grad)
        extrusion (map #(g/extrude %1 {:depth %2 :mesh (gm/gmesh)})
                       hexagons
                       gradient)
        solids (reduce g/into extrusion)]
    solids))

(save-stl "ring.stl" (hex-grid))
