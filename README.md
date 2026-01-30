# SkinOmicsExplorer
---
title: "SkinOmicsExplorer"
output:
  github_document:
    toc: true
    toc_depth: 3
---

# SkinOmicsExplorer

SkinOmicsExplorer is an interactive R Shiny portal for exploring and querying the multi-omics datasets generated in:

> **Yuqing et al.**  
> *A Spatially Coordinated Keratinocyte–Fibroblast Circuit Recruits MMP9⁺ Myeloid Cells to Drive IFN-I-Driven Inflammation in Photosensitive Autoimmunity*  
> *(accepted in Nature Immunology)*

The portal integrates:

- single-cell RNA sequencing  
- spatial transcriptomics (seqFISH)  
- bulk RNA-seq  
- targeted proteomics (OLINK & NULISA)

to enable interactive visualization of gene and protein expression across diseases, cell types, spatial contexts, and experimental perturbations.

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
  👉 https://github.com/mgarber/AddOns  

- **scSpatial** — spatial transcriptomics processing and visualization tools  
  👉 https://github.com/Yuqing66/scSpatial  

These repositories provide core functionality for interactive plotting, spatial rendering, and multi-omics integration.

---

## Running 
Docker images are available for easy running of the portal without installing dependencies 
```bash
docker pull yuqing987/r-skinomicsexplorer-arm

docker run -p 8888:8888 \
  -v "./shinyApp_content":/home/app_data/ \
  -it yuqing987/r-skinomicsexplorer-arm:latest
```
## Citation
Yuqing et al.
A Spatially Coordinated Keratinocyte–Fibroblast Circuit Recruits MMP9+ Myeloid Cells
to Drive IFN-I-Driven Inflammation in Photosensitive Autoimmunity.
Nature Immunology (accepted)
