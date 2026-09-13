# UP Local Government Directory extracts

Downloaded from the LGD portal (<https://lgdirectory.gov.in>) on **2026-03-25**.

**These are dated snapshots, not reproducible downloads.** The portal's contents
change as bodies are created, merged and renamed, and its exports carry a
download timestamp rather than a data version. Re-downloading tomorrow gives a
different file with a different name. These archived snapshots record what LGD said on that date. Current analysis
uses the hash-pinned directory and geographic bridge from `local_elections_up`;
the local extraction and matching scripts have been removed.

Compare the sha256 below rather than the filename to tell whether contents match.

## Files

| file | size | read by | notes |
| --- | ---: | --- | --- |
| `up_block_panchayat.xls` | 44 MB | — | historical extraction source |
| `up_village_gp_mapping.xls.gz` | 3.7 MB | — | 146 MB raw |
| `priWards2026:03:25:06:50:56:981.xls.gz` | 10.6 MB | — | 737 MB raw; one row per ward of every UP gram panchayat |

`priWards` is ward-level: `Local Body Code, Local Body Name, Local Body Type,
District, Intermediate Parent, Ward Code, Ward Number, Ward Name (English),
Ward Name (Local)`. The GP directory now supplied by `local_elections_up` was extracted from
`up_block_panchayat.xls`, not this ward-level file.

The `.gz` files are SpreadsheetML XML, which compresses 40--70x. Every one was
verified to decompress byte-identically to the original before that original was
removed. Read one with `gzcat file.xls.gz`, or in R with
`readLines(gzfile(...))`.

## Timestamped duplicates removed

The portal names its exports with a download timestamp; two were kept alongside
renamed copies, so each was stored twice. The duplicates were removed after
verifying they were byte-identical:

| kept | the portal filename it arrived as | sha256 |
| --- | --- | --- |
| `up_block_panchayat.xls` | `priLbSpecificState2026:03:25:06:49:53:420.xls` | `f6a49d77b389d239ec5e1e86a8d0f2eacbd9f898707b606f67bbed79b9abaf4b` |
| `up_village_gp_mapping.xls.gz` | `villageGramPanchayatMapping2026:03:25:06:51:18:059.xls` | `ec4979fa7f741d6e07689fe2a7d72b4dafd4aa7cc48e8514daca605a7931b5db` |

(The sha256 above are of the **uncompressed** contents.)
