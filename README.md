# GET Detection Analyzer

**Version 1.0**  
**Author:** Brian Benitez  
**GitHub:** https://github.com/BBenitez95

![R](https://img.shields.io/badge/R-4.0%2B-blue)
![License](https://img.shields.io/badge/License-MIT-yellow)

---

## Overview

The GET Detection Analyzer identifies respiratory thresholds from graded exercise test data using piecewise linear regression.

Two respiratory thresholds are detected:
- **Gas Exchange Threshold (GET)**: Transition from moderate to heavy intensity
- **Respiratory Compensation Point (RCP)**: Transition from heavy to severe intensity

## Installation & Use

1. Download `GET_Detection_Analyzer.R`
2. Open in R or RStudio
3. Run the entire script

Required packages will be automatically installed on first run.

## Data Format

Input files must contain four columns:
1. **Time**: Numeric values (seconds or minutes)
2. **VO₂**: Oxygen uptake (L/min or mL/min)
3. **VCO₂**: Carbon dioxide production (L/min or mL/min)
4. **VE**: Minute ventilation (L/min)

Supported formats: CSV, TXT, XLSX

Headers are optional and will be automatically detected.

Example:
```
Time,VO2,VCO2,VE
0.00,0.35,0.30,12.5
0.08,0.42,0.36,14.2
0.17,0.48,0.41,15.8
...
```

## Detection Methods

### Gas Exchange Threshold (GET)

**V-slope Method**: Detects the breakpoint in the linear relationship between VCO₂ and VO₂.

The GET represents the exercise intensity at which VCO₂ production begins to increase disproportionately relative to VO₂, indicating the onset of sustained anaerobic metabolism and the transition from moderate to heavy-intensity exercise.

**Implementation**: Piecewise linear regression with two segments fit to VCO₂ vs VO₂ data. The breakpoint represents GET.

### Respiratory Compensation Point (RCP)

**Ventilatory Method**: Detects the breakpoint in the linear relationship between VE and VCO₂.

The RCP represents the exercise intensity at which minute ventilation increases disproportionately relative to VCO₂, indicating metabolic acidosis and hyperventilatory compensation. Marks the transition from heavy to severe-intensity exercise.

**Implementation**: Piecewise linear regression with two segments fit to VE vs VCO₂ data. The breakpoint represents RCP.

## Analysis Procedure

1. Import graded exercise test data to exhaustion
2. Configure VO₂peak calculation window (10, 20, or 30 seconds)
3. Apply optional pre-processing (smoothing, outlier removal)
4. Detect GET using complete dataset
5. Detect RCP using complete dataset
6. Verify physiological validity (GET_VO₂ < RCP_VO₂)
7. Export threshold parameters and fitted models

## VO₂peak Determination

VO₂peak is calculated as the highest moving average VO₂ over a specified time window. For each time point *t*, all VO₂ values occurring within the interval [*t* - window/2, *t* + window/2] are averaged. VO₂peak is defined as the maximum of these averaged values across all time points.

Window options: 10, 20, or 30 seconds (default: 30 s)

## Output

### Threshold Detection Table

- VO₂peak (L/min and 100% reference)
- GET: VO₂ at threshold (L/min and % of peak)
- RCP: VO₂ at threshold (L/min and % of peak)
- Breakpoint coordinates for each threshold
- Pre- and post-breakpoint slopes
- R² (goodness of fit, typically >0.95)

### Exported Files

**Plot (PNG)**:
- Data points with fitted piecewise linear model
- Vertical line marking breakpoint location
- Threshold annotation with VO₂ and % of peak
- Outliers shown as red X's (if removed)

**Excel file** with three sheets:
1. **Threshold Results**: VO₂, % of peak, breakpoint coordinates, slopes, R² for GET and RCP
2. **Analysis Settings**: VO₂peak calculation method, pre-processing options, outlier removal parameters
3. **Raw Data**: Time series data for all variables

## Pre-processing Options

### Smoothing

Applies a centered moving average filter with window size equal to 5% of the data length (minimum 3 points). Reduces high-frequency noise while preserving underlying physiological responses.

### Outlier Removal

Uses locally weighted scatterplot smoothing (LOWESS, span = 0.1) to estimate baseline trends for VO₂, VCO₂, and VE independently. Data points where any variable exceeds the specified standard deviation threshold (default: 3 SD) from its baseline are removed prior to threshold detection.

## Interpretation

### Expected Threshold Ranges

- **GET**: Typically 50–70% of VO₂peak
- **RCP**: Typically 80–90% of VO₂peak

### Physiological Validation

GET must occur at a lower VO₂ than RCP. If RCP ≤ GET, the software displays a warning indicating the results are physiologically implausible and should be verified.

## References

Beaver, W. L., Wasserman, K., & Whipp, B. J. (1986). A new method for detecting anaerobic threshold by gas exchange. *Journal of Applied Physiology*, 60(6), 2020–2027.

Wasserman, K., Hansen, J. E., Sue, D. Y., Stringer, W. W., & Whipp, B. J. (2012). *Principles of Exercise Testing and Interpretation* (5th ed.). Lippincott Williams & Wilkins.

## Citation

```
Benitez, B., Succi, PJ., & Snell, E. (2025). GET Detection Analyzer: Automated threshold 
detection for graded exercise tests (Version 1.0) [Software]. 
https://github.com/BBenitez95/GET_Detection_Analyzer
```

## License

MIT License

Copyright (c) 2025 Brian Benitez, Erik Snell

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

## Contact

Dr. Brian Benitez  
Stetson University  
Health Sciences  
Email: bbenitez@stetson.edu
