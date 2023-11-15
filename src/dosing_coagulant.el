(defvar *coagulant-concentration* 0.8
  "Default concentration of the coagulant")

(defvar *flow-rate* 1.2
  "Default flow rate of the line")

(defun set-coagulant-concentration (concentration)
  "Set the concentration of the coagulant."
  (setq *coagulant-concentration* concentration))

(defun set-flow-rate (flow-rate)
  "Set the flow rate of the line."
  (setq *flow-rate* flow-rate))

(defun calculate-flow (volumetric-flow concentration)
  "Calculate the flow required from the floculant system."
  (let* ((adjusted-concentration (* concentration *coagulant-concentration*))
         (adjusted-flow (* volumetric-flow *flow-rate*))
         (volumetric-flow-sulfate-aluminum (/ adjusted-flow adjusted-concentration)))
    (message "The volumetric flow of sulfate aluminum is %f" volumetric-flow-sulfate-aluminum)))

(calculate-flow 10 0.5)
