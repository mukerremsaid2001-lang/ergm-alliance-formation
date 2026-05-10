# ERGM Analysis of Alliance Formation (1965–1985)

A quantitative network analysis examining how **nuclear weapon status** and **military expenditure** influence the formation of formal military alliances during the Cold War.

> Final project for POLS 370, Boğaziçi University. Uses Exponential Random Graph Models (ERGMs) on Correlates of War data across three snapshots: 1965, 1975, and 1985.

---

## Research Question

To what extent do power-related attributes — nuclear capability and military expenditure — influence the formation of mutual defense pacts between states?

## Hypotheses

- **H1:** Nuclear Weapon States (NWS) are more likely to form alliances than Non-Nuclear Weapon States (NNWS).
- **H2:** States with higher military expenditure are more likely to form alliances.

## Data

Two datasets from the [Correlates of War project](https://correlatesofwar.org/):

| Dataset | Description | Source |
|---|---|---|
| `alliance_v4.1_by_member_yearly.csv` | Formal alliances among states, 1816–2012 (mutual defense pacts, non-aggression treaties, ententes) | [Formal Alliances v4.1](https://correlatesofwar.org/data-sets/formal-alliances/) |
| `NMC-60-abridged.csv` | National Material Capabilities (military expenditure, personnel, energy consumption, etc.) | [NMC v6.0](https://correlatesofwar.org/data-sets/national-material-capabilities/) |

Nuclear status and NATO / Warsaw Pact membership were coded manually for each year.

## Methodology

Networks were constructed for 1965, 1975, and 1985, where:
- **Nodes** = states
- **Edges** = mutual defense pacts between two states

Three ERGM specifications were estimated for each year:
1. `edges + nodecov(nuclear)` — baseline
2. `edges + nodecov(nuclear) + nodecov(milex)` — adding military expenditure
3. `gwnsp(0.5, fixed = TRUE)` — testing transitive closure

## Key Findings

### Nuclear status (testing H1)

| Year | Coefficient | p-value | Effect |
|------|-------------|---------|--------|
| 1965 | −0.124 | 0.594 | Not significant |
| 1975 | −0.472 | 0.013 | Significant negative |
| 1985 | −0.887 | <0.001 | Stronger negative |

**H1 is rejected.** Contrary to expectations, nuclear states were not more likely to form alliances — and from 1975 onward, they were significantly *less* likely to do so. This is consistent with deterrent autonomy: nuclear states may rely less on alliance commitments.

### Military expenditure (testing H2)

| Year | Coefficient | p-value | Effect |
|------|-------------|---------|--------|
| 1965 | +2.41e−08 | <0.001 | Significant positive |
| 1975 | +1.13e−08 | <0.001 | Significant positive |
| 1985 | +6.68e−09 | <0.001 | Significant positive |

**H2 is confirmed.** Across all three years, higher military expenditure is strongly associated with alliance formation, consistent with realist accounts of power-based alignment.

### Transitive closure (gwnsp)

The gwnsp term is negative and highly significant in all three years (1965: −0.66, 1975: −0.68, 1985: −0.91). Countries sharing mutual allies but not directly allied were *less* likely to form a direct alliance — contradicting "friend of my friend" balance theories. Alliance networks appear shaped more by bloc exclusivity than by triadic closure.

## How to Run

### Requirements
- R (≥ 4.0)
- R packages: `statnet`, `igraph`, `intergraph`, `ergm`, `readr`, `dplyr`, `tidyverse`

```r
install.packages(c("statnet", "igraph", "intergraph", "ergm", 
                   "readr", "dplyr", "tidyverse"))
```

### Steps
1. Download the two CSV files from Correlates of War (links above) and place them in the working directory.
2. Open `pols370_final_alliance.R` in RStudio.
3. Run the script. ERGM estimation may take several minutes per model.

## Repository Structure
```
├── pols370_final_alliance.R    # Main analysis script
└── README.md                    # This file
```


## Notes

- Some auxiliary code (edge list generation, vertex list construction) was written with AI assistance and is marked with `***` in the script comments.
- The "Non-Aligned" label in the visualizations is used loosely to group strategically important states without mutual defense pacts; it does not refer to the historical Non-Aligned Movement. Bloc affiliation is not used in hypothesis testing.
- 
📊 **[View the presentation slides](POLS370%20FINAL%20FINAL.pdf)** — visual summary with network plots and key findings.

> **Note on numerical results:** The slides report coefficients from earlier model specifications that included `nodefactor("bloc")` as a control variable. The README reports the revised models without this control (see script comments). The substantive conclusions — H1 rejected, H2 confirmed — are identical across specifications.
> 
## Author

Mükerrem Said Özdemir — Boğaziçi University, POLS 370, May 2025
