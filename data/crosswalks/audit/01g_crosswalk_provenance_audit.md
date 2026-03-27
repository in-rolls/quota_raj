# Crosswalk Provenance Audit

Generated: 2026-03-26 17:00:29

## Integrity Summary
- Files audited: 8
- Missing files: 0
- Files with non-unique keys: 0
- Files with LGD validity failures: 0

## LGD Validity Checks

| File | Check | Invalid Rows |
|---|---|---:|
| data/crosswalks/active/up_block_xwalk.csv | lgd_block_code in lgd_up_blocks$block_code | 0 |
| data/crosswalks/active/up_district_xwalk.csv | lgd_district in lgd_up_blocks$zp_name | 0 |
| data/crosswalks/active/up_district_xwalk.csv | lgd_zp_code in lgd_up_blocks$zp_code | 0 |
| data/crosswalks/active/raj_samiti_xwalk.csv | lgd_block_code in lgd_raj_block_gp$block_code | 0 |

## Notes
- `data/crosswalks/active/raj_district_xwalk.csv` and `data/crosswalks/active/raj_samiti_std.csv` are the active standardization crosswalks used for Rajasthan panel keys.
- `data/crosswalks/active/raj_samiti_xwalk.csv` is the active LGD block matching crosswalk for Rajasthan.
- `data/crosswalks/active/up_block_xwalk.csv` is the active LGD block matching crosswalk for UP.
- LGD hierarchy reference files are read from `data/lgd/processed/`.
