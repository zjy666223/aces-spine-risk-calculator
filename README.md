# ACEs Spine Degenerative Disease Risk Calculator

This folder contains a static, GitHub Pages-ready web calculator for the ACEs and incident spine degenerative disease project.

## Contents

- `index.html`: main web interface.
- `assets/styles.css`: responsive dashboard styling.
- `assets/app.js`: risk calculation and local explanation logic.
- `assets/model-data.js`: browser-readable Cox model parameters.
- `model/cox_risk_model.json`: model parameters in JSON.
- `model/extract_cox_web_model.R`: reproducible script that refits the nomogram-aligned Cox model and exports the web model.

## Model Source

The calculator uses the same Cox predictor set as the current nomogram workflow:

- ACEs score
- age
- sex
- BMI
- education
- smoking status
- household income
- physical activity
- TDI

The exported validation references are:

- Cox test C-index: 0.6657
- Best machine-learning test AUC: 0.669
- XGBoost test AUC: 0.667

## How To Use

Open `index.html` directly in a browser. No server is required.

To refresh model parameters after rerunning the analysis pipeline:

```bash
Rscript model/extract_cox_web_model.R
```

The page is a research visualization and should not be used as a clinical decision tool.
