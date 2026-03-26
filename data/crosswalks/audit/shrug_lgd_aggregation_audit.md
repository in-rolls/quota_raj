# SHRUG-LGD Aggregation Audit

Generated: 2026-03-26 13:46:59

| State | LGD Codes | Villages | Multi-village LGD | % Multi-village | Median villages | Max villages |
|---|---:|---:|---:|---:|---:|---:|
| andaman nicobar islands | 70 | 317 | 55 | 78.6 | 3.0 | 36 |
| andhra pradesh | 18757 | 26090 | 3470 | 18.5 | 1.0 | 31 |
| arunachal pradesh | 1668 | 3294 | 831 | 49.8 | 1.0 | 13 |
| assam | 2188 | 18869 | 2142 | 97.9 | 7.0 | 79 |
| bihar | 7938 | 39042 | 7011 | 88.3 | 4.0 | 148 |
| chhattisgarh | 11521 | 19375 | 5159 | 44.8 | 1.0 | 13 |
| dadra nagar haveli | 19 | 67 | 18 | 94.7 | 3.0 | 10 |
| daman diu | 13 | 23 | 5 | 38.5 | 1.0 | 4 |
| goa | 187 | 375 | 78 | 41.7 | 1.0 | 16 |
| gujarat | 13824 | 17798 | 2177 | 15.7 | 1.0 | 24 |
| haryana | 5714 | 6316 | 511 | 8.9 | 1.0 | 5 |
| himachal pradesh | 3506 | 17359 | 3074 | 87.7 | 4.0 | 35 |
| jammu kashmir | 3408 | 6099 | 1450 | 42.5 | 1.0 | 11 |
| jharkhand | 4155 | 29529 | 3882 | 93.4 | 6.0 | 42 |
| karnataka | 6088 | 27237 | 5168 | 84.9 | 3.0 | 343 |
| kerala | 861 | 1384 | 347 | 40.3 | 1.0 | 22 |
| lakshadweep | 10 | 12 | 2 | 20.0 | 1.0 | 2 |
| madhya pradesh | 23044 | 51892 | 15419 | 66.9 | 2.0 | 68 |
| maharashtra | 27664 | 40168 | 7774 | 28.1 | 1.0 | 29 |
| manipur | 145 | 402 | 101 | 69.7 | 2.0 | 10 |
| meghalaya | 1 | 1 | 0 | 0.0 | 1.0 | 1 |
| odisha | 6788 | 47305 | 6567 | 96.7 | 6.0 | 46 |
| puducherry | 72 | 92 | 18 | 25.0 | 1.0 | 3 |
| punjab | 11193 | 12088 | 744 | 6.6 | 1.0 | 8 |
| rajasthan | 11117 | 39442 | 9234 | 83.1 | 3.0 | 30 |
| sikkim | 195 | 412 | 132 | 67.7 | 2.0 | 5 |
| tamil nadu | 11428 | 15170 | 2512 | 22.0 | 1.0 | 13 |
| tripura | 318 | 384 | 57 | 17.9 | 1.0 | 5 |
| uttar pradesh | 34123 | 58902 | 14550 | 42.6 | 1.0 | 176 |
| uttarakhand | 7720 | 14980 | 4092 | 53.0 | 2.0 | 14 |
| west bengal | 3316 | 37952 | 3210 | 96.8 | 9.0 | 79 |

Aggregation policy:
- Count-like numeric variables: sum across villages in the LGD GP.
- Share/rate variables (name pattern contains share/ratio/rate/pct/prop): weighted mean using `pc01_pca_tot_p`, fallback to unweighted mean.
