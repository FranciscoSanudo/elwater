;; Author: Francisco J. Sanudo
;; Description: Water Treatment Dosification with Lisp
;;
;; This script calculates the flow required from the floculant system in water treatment.
;; It takes into account the volumetric flow of water, the desired concentration of floculant,
;; the concentration of the coagulant, and the flow rate of the line to calculate the volumetric
;; flow of sulfate aluminum. It also calculates the mass flow rate required for sulfate aluminum
;; and the tank size with sufficient retention time for a homogeneous mix.
;;
;; Usage:
;;   - Call the 'calculate-flow' function with the volumetric flow and concentration parameters
;;     to calculate the volumetric flow of sulfate aluminum.
;;   - Call the 'calculate-mass

(defvar *coagulant-density* 1.2
  "Density of the coagulant in g/cm^3")

(defvar *retention-time* 30
  "Retention time in minutes")

(defun set-coagulant-density (density)
  "Set the density of the coagulant."
  (setq *coagulant-density* density))

(defun set-retention-time (time)
  "Set the retention time."
  (setq *retention-time* time))

(defun calculate-mass-flow (volumetric-flow concentration)
  "Calculate the mass flow rate required for sulfate aluminum."
  (let* ((volumetric-flow-sulfate-aluminum (calculate-flow volumetric-flow concentration))
         (mass-flow-rate (* volumetric-flow-sulfate-aluminum *coagulant-density*)))
    (message "The mass flow rate required for sulfate aluminum is %f g/min" mass-flow-rate)))

(defun calculate-tank-size (volumetric-flow concentration)
  "Calculate the tank size with sufficient retention time for a homogeneous mix."
  (let* ((volumetric-flow-sulfate-aluminum (calculate-flow volumetric-flow concentration))
         (mass-flow-rate (* volumetric-flow-sulfate-aluminum *coagulant-density*))
         (tank-volume (/ mass-flow-rate (* *retention-time* 60))))
    (message "The tank size required for a retention time of %d minutes is %f cm^3" *retention-time* tank-volume)))

(set-coagulant-density 1.4)
(set-retention-time 45)
(calculate-mass-flow 10 0.5)
(calculate-tank-size 10 0.5)
