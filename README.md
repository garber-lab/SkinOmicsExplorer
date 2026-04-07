# SkinOmicsExplorer


SkinOmicsExplorer is an interactive R Shiny portal for exploring and querying the multi-omics datasets generated in:

> **Yuqing et al.**  
*A Spatially Coordinated Keratinocyte–Fibroblast Circuit Recruits MMP9⁺ Myeloid Cells to Drive IFN-I-Driven Inflammation in Photosensitive Autoimmunity*  
Yuqing Wang, Khashayar Afshari, Nazgol-Sadat Haddadi, Carolina Salomão Lopes, Chee-Huat Linus Eng, Nuria Martinez, Leah Whiteman, Ksenia S Anufrieva, Kevin Wei, Kirsten Frieda, Stefania Gallucci, Misha Rosenbach, Ruth Ann Vleugels, John E Harris, Mehdi Rashighi, Manuel Garber
bioRxiv 2025.08.19.670635; doi: https://doi.org/10.1101/2025.08.19.670635

The portal integrates:

- single-cell RNA sequencing  
- spatial transcriptomics (seqFISH)  
- bulk RNA-seq  
- targeted proteomics (OLINK & NULISA)

to enable interactive exploration of gene and protein expression across diseases, cell types, spatial contexts, and experimental perturbations, with visualization mirroring those used in our paper.

A hosted version of this application can be accessed at [https://CD14photosensitivity.umassmed.edu](https://CD14photosensitivity.umassmed.edu)

---

##  Key features

- Interactive scRNA-seq UMAP embeddings and gene expression plots  
- Spatial visualization of seqFISH data across disease and UV perturbations  
- Targeted proteomics visualization for in vivo and in vitro experiments  
- Bulk RNA-seq perturbation analysis  
- Publication-ready plot export (PNG/PDF)  
- Multi-gene heatmap generation  

---

## Supporting repositories

SkinOmicsExplorer builds on custom visualization and analysis frameworks developed in:

- **AddOns** — reusable Shiny modules and visualization utilities  
   https://github.com/garber-lab/AddOns  

- **scSpatial** — spatial transcriptomics processing and visualization tools  
   https://github.com/garber-lab/scSpatial  

These repositories provide core functionality for interactive plotting, spatial rendering, and multi-omics integration.

---

## Running 
Docker images are available for easy running of the portal without installing dependencies 
```bash
docker pull ghcr.io/garber-lab/skinomicsexplorer:latest

docker run -p 8888:8888 \
  -v "./shinyApp_content":/home/app_data/ \
  -it ghcr.io/garber-lab/skinomicsexplorer:latest
```

Then open your browser at:
```bash
http://localhost:8888
```

## Resource requirements
For small and moderate datasets, default Docker settings may be sufficient.
However, larger datasets (e.g., GSE179633 single-cell RNA-seq) require substantially more memory.

Common issue

At  least on macOS (Docker Desktop), the default configuration limits available memory, which can cause:

- sudden app crashes
- no visible error messages
- instability when loading large datasets

This is typically due to out-of-memory (OOM) conditions.

### Recommended Docker Desktop Settings (Tested)
To ensure stable performance with large datasets, we recommend:

- **CPU**: 12 cores
- **Memory**: 12 GB
- **Swap**: 2 GB

These settings were tested and found sufficient for interactive use with large single-cell datasets such as GSE179633.

## Citation
Yuqing et al.
A Spatially Coordinated Keratinocyte–Fibroblast Circuit Recruits MMP9+ Myeloid Cells
to Drive IFN-I-Driven Inflammation in Photosensitive Autoimmunity.
Nature Immunology (accepted)
