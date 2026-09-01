
> ### Fork notice
>
> This is a personal fork of the original
> [AMMonitor](https://code.usgs.gov/vtcfwru/ammonitor/-/tree/master)
> (USGS/GitLab, branch `AMMonitor2.2`), maintained by its owner. It exists mainly as a GitHub
> backup/mirror of the GitLab-hosted original, with local customizations
> layered on top. Full detail in [CUSTOMIZATIONS.md](CUSTOMIZATIONS.md); in
> short:
>
> - **BirdNET integration** -- `birdsDetect()`, `birdSpeciesList()` /
>   `birdSpeciesAdd()` / `birdSpeciesRemove()`, `registerBirdNETModel()`,
>   `registerBirdNETSpecies()`: new exported functions adding
>   [BirdNET](https://birdnet-team.github.io/birdnetR/) (via `birdnetR`) as
>   a detection model, replacing an older Python + CSV-import workflow.
> - **Multicore support** -- `scoresDetect()` and `birdsDetect()` both
>   accept a `numCores` argument to process recordings concurrently.
> - **Automatic throughput reporting** -- both detection functions report
>   elapsed time, throughput, and real-time-speed multiplier when
>   `showProgress = TRUE`.
> - **Bug fixes** -- a double-slash in uploaded media file paths when
>   uploading to an S3 bucket root; a `birdsDetect()` failure on recordings
>   whose filenames contain spaces.
> - **Shiny app customizations** -- per-recording comments, manual
>   detection-count overrides, AudioMoth metadata capture on visit
>   registration, and Taxon Model Outputs table UI improvements.

# AMMonitor

**Remote monitoring of biodiversity in an adaptive framework**

<IMG SRC= 'https://img.shields.io/static/v1?label=&message=AMMonitor&color=<"green">'>
<IMG SRC= 'https://img.shields.io/static/v1?label=&message=R&color=<"green">'>
<IMG SRC= 'https://img.shields.io/static/v1?label=&message=Wildlife Monitoring&color=<"green">'>
<IMG SRC= 'https://img.shields.io/static/v1?label=&message=Adaptive Management&color=<"green">'>
<IMG SRC= 'https://img.shields.io/static/v1?label=&message=Acoustic Monitoring&color=<"green">'>
<IMG SRC= 'https://img.shields.io/static/v1?label=&message=Camera Monitoring&color=<"green">'>
<IMG SRC= 'https://img.shields.io/static/v1?label=&message=VTCFWRU&color=<"green">'>

<IMG SRC="https://code.usgs.gov/vtcfwru/ammonitor/raw/master/inst/extdata/figs/ammonitor-footer.png" alt="ammonitor_footer">

## Authors

Laurence Clarfeld, Caroline Tang, Kaitlin Huber, Cathleen Balantic, Kayley Dillon, and Therese Donovan


## Point of contact: 

Therese Donovan (tdonovan@usgs.gov)
Laurence Clarfeld (lclarfeld@usgs.gov)

## Information

- Repository Type: Program R scripts
- Year of Origin:  2022
- Year of Version: 2025
- Version: 2.2.0
- Digital Object Identifier (DOI): https://doi.org/10.5066/P13MRDRV
- USGS Information Product Data System (IPDS) no.: IP-180223 (internal agency tracking)

## Suggested Citation for Software

Clarfeld, L., Tang, C., Huber, K., Balantic, C., & Donovan, T. (2025). AMMonitor 2: Remote monitoring of biodiversity in an adaptive framework in R. Methods in Ecology and Evolution, 16(3), 477-485. https://doi.org/10.1111/2041-210X.14487

and

Clarfeld, L., C. Tang, K. Huber, K. Dillon, C. Balantic, and T. Donovan. AMMonitor: Remote monitoring of biodiversity in an adaptive framework. Version 2.2.0: U.S. Geological Survey software release. Reston, VA. https://doi.org/10.5066/P13MRDRV


## Overview

Amid climate change and rapidly shifting land uses, effective methods for monitoring wildlife are critical to support scientifically-informed resource management decisions. As such, monitoring is motivated by ecological hypotheses or natural resource management objectives. The practice of using Autonomous Monitoring Units (AMUs) such as trail cameras or audio recorders to monitor wildlife species has grown immensely in the past decade, with monitoring projects spanning species from birds, to bats, amphibians, insects, terrestrial mammals, and marine mammals.

AMUs can be deployed for long periods of time to collect massive amounts of audio and photographic data. However, the data management requirements can be immense, leaving researchers buried under terabytes of data. A monitoring program is a collection of people, equipment, monitoring locations, location characteristics, research objectives, and data files, with multiple moving parts to manage. Without a comprehensive framework for efficiently moving from raw data collection to results and analysis, monitoring programs are limited in their capacity to characterize ecological processes and inform management decisions in a timely manner.

**AMMonitor** stands for "monitoring for adaptive management". It is an analysis ecosystem (R + SQLite + media storage) that seamlessly incorporates wildlife monitoring data, species distribution modeling, and decision tools.  The analysis ecosystem is characterized by:

1.	Continuous stream of data collection on a variety of wildlife taxa.  Autonomous monitoring units (AMUs) such as trail cameras or audio recorders allow the remote capture of digital files that form the backbone of wildlife monitoring.  AMUs can be deployed at small or large scales, over short or long-time frames, and are inexpensive relative to human-based surveys.

2.	Standardized yet flexible data and metadata infrastructure.  Because many terabytes of monitoring data are collected by AMUs, the data management requirements of an AMU-based monitoring effort are immense.  A standardized data infrastructure is critical to store files with strict metadata standards that can be easily incorporated into open access repositories. 

3.	Rapid analysis.  Vast quantities of AMU data may be collected in a short amount of time, rendering manual inspection of files for target wildlife species impractical.  Accordingly, machine learning (ML) algorithms can be used to automatically detect wildlife species in recordings and photos, avoiding the time-consuming task of manual annotation. ML outputs can be seamlessly incorporated into species distribution models and updated as new data are collected via continuous monitoring. 

Each *AMMonitor* project utilizes the same approach, keyed to Figure 1:

<p>
<center><IMG SRC="https://code.usgs.gov/vtcfwru/ammonitor/-/raw/AMMonitor2.1/inst/tutorials/intro/www/wheel.png" alt="Figure 1. The general AMMonitor framework begins with basic
research hypotheses or applied resource management objectives." width=400 />
<br><i>Figure 1. The general AMMonitor framework begins with basic 
research hypotheses or applied resource management objectives</i>.
</center>
</p>

<br>
<br>


<p>
(A) Wildlife data are collected by AMUs such as trail cameras and audio recorders to monitor mammals, birds, insects, amphibians, and bats;  

(B) R functions upload media files to a local machine or to the cloud, with an option of permanent archiving in the USGS ScienceBase repository; 

(C) Media files can be tagged by humans and/or analyzed with machine learning models that automate the species detection process (e.g., there is a 0.65 probability that a white-tailed deer is in this image or a 0.8 probability that this audio signal was issued by a Black-capped Chickadee); 

(D) Raw and post-processed data are maintained in the project’s SQLite database; 

(E) R functions and scripts permit rapid, reproducible analyses of species distribution models and resulting maps, regularly updated along with metadata as new data are analyzed;  

(F) These products can be the building blocks of a decision support tool, continually updated as knowledge evolves, enabling decisions to be made with the most current information. 
</p>

We created the first iteration of *AMMonitor* (Balantic and Donovan 2020, 2021) for the Bureau of Land Management to monitor high priority wildlife across the southern California Solar Energy Zone (SEZ). The second iteration of *AMMonitor* significantly expands on the first (Clarfeld, Tang, Huber, Balantic, Dillon, and Donovan).  Primary developments include:

- A user-friendly interface for running analyses and working with data via R Shiny;
- Built-out methods for wildlife monitoring with trail cameras;
- A friendly R Shiny image and audio tagger, allowing humans to label data captured within media files;
- Support for a variety of cloud-based storage options for storing media files;
- Updated R functions that streamline data analysis;
- Improved ability to develop template-based audio machine-learning models;
- Ability to incorporate existing machine learning models, such as Microsoft's MegaDetector or Cornell Lab of Ornithology's BirdNET;
- Functions that aid in creating mobile applications such as Survey123 and Google AppSheets for collecting site visit data;
- Ability to collaborate directly with the USGS AMBER Network (Alliance for Monitoring Biodiversity and Ecosystems Remotely) using cloud-based resources for rapid analysis (requires a data-sharing agreement and fee structure);
- A suite of *learnr* tutorials that introduce users to the *AMMonitor* 2.0 software and approach.   


## Foundational papers:

- Clarfeld, L., Tang, C., Huber, K., Balantic, C., & Donovan, T.  2025.  AMMonitor 2: Remote monitoring of biodiversity in an adaptive framework in R. Methods in Ecology and Evolution. https://doi.org/10.1111/2041-210X.14487
- Balantic, C., and T. Donovan.  2020.  AMMonitor: Remote monitoring of biodiversity in an adaptive framework with R.  Methods in Ecology and Evolution. https://doi.org/10.1111/2041-210X.13397
- Balantic, C. M., & Donovan, T. M. 2019. Temporally adaptive acoustic sampling to optimize detection across a suite of focal species. Ecology and Evolution 9:10582– 10600. https://doi.org/10.1002/ece3.5579
- Balantic, C. M., & Donovan, T. M. 2019. Statistical learning mitigation of false positives from template‐detected data in automated acoustic wildlife monitoring. Bioacoustics 292:96–321. https://doi.org/10.1080/09524622.2019.1605309
- Balantic, C. M., & Donovan, T. M. 2019. Dynamic wildlife occupancy models using automated acoustic monitoring data. Ecological Applications. https://doi.org/10.1002/eap.1854
- Donovan, T. M., & Katz, J. E. 2018. AMModels: An R package for storing models, data, and metadata to facilitate adaptive management. PLoS ONE 13(2), e0188966. https://doi.org/10.1371/journal.pone.0188966
- Katz, J., Hafner, S. D., & Donovan, T. M. 2016. Tools for automated acoustic monitoring within the R package monitoR. Bioacoustics, 25, 197– 210. https://doi.org/10.1080/09524622.2016.1138415
- Katz, J., S. Hafner, and T. Donovan.  2016. Assessment of error rates in acoustic monitoring with the R package monitoR. Bioacoustics 25(2):177-196. DOI:10.1080/09524622.2015.1133320
- Donovan. T. M., C. Balantic, J. Katz, M. Massar, R. Knutson, K. Duh, P. W. Jones, K. Epstein, J. Lacasse-Roger, and J. Dias. 2021. Remote ecological monitoring with smartphones and Tasker. Journal of Fish and Wildlife Management 12:163–173.  https://doi.org/10.3996/JFWM-20-071
- Clarfeld LA, Sirén AP, Mulhall BM, Wilson TL, Bernier E, Farrell J, Lunde G, Hardy N, Gieder KD, Abrams R, Staats S. 2023.  Evaluating a tandem human-machine approach to labelling of wildlife in remote camera monitoring. Ecological informatics 77:102257. https://doi.org/10.1016/j.ecoinf.2023.102257
- Clarfeld, L. A., Gieder, K. D., Abrams, R., Bernier, C., Cahill, J., Staats, S., ... & Donovan, T. M. 2025. Two-stage models improve machine learning classifiers in wildlife research: A case study in identifying false positive detections of Ruffed Grouse. Ecological Informatics, 103166. https://doi.org/10.1016/j.ecoinf.2025.103166


## Installation

For the released version:

```
# increase the timeout
getOption('timeout')
options(timeout = 300)

# install with the remotes package
remotes::install_gitlab(
  repo = "vtcfwru/ammonitor@2.2.0",  
  auth_token = Sys.getenv("GITLAB_PAT"),  
  host = "code.usgs.gov",  
  build_vignettes = FALSE,  
  dependencies = TRUE,
  upgrade = "never")

```

Alternatively, uses may download the compiled software from https://code.usgs.gov/vtcfwru/ammonitor/-/releases.

On that page, Windows users can download ammonitor_2.2.0.zip, while Mac/Linux can download ammonitor_2.2.0.tar.gz.  Then, in R Studio, click on the Install button on the Packages tab, and select Install From Package Archive File, and navigate to the downloaded file.

## Getting Started

Load *AMMonitor* by typing:

```
library(AMMonitor)
```

The easiest way to see an *AMMonitor* project in action is to create the "mini-demo" project, and launch the shiny interface:

```
# create the mini project and capture the filepath
fp <- AMMonitor::ammCreateMiniDemo(tempdir())

# launch the shiny app
AMMonitor::launchApp(fp)
```

## Tutorials

*AMMonitor* comes with several learnr tutorials that provide step-by-step guidance. To see a list of available tutorials, use the available_tutorials() function from learnr:

```
learnr::available_tutorials(package = "AMMonitor")
```

Currently, 20 tutorials are available,  listed below in a suggested order of completion:

Available tutorials:

1. **intro**.  This is the first tutorial to complete. It introduces what a *learnr* tutorial is and gives an overview of the available tutorials in the R package, *AMMonitor*.
2. **getting_started**. A broad overview of *AMMonitor* and how to create an *AMMonitor* project on your machine.
3. **database**.	Introduces and overviews the AMMonitor SQLite database.
4. **dbdictionary**.	Introduces the dbdictionary, lists, and shinytable tables.
5. **dbsummary**. Introduces the dbGetSummaryData() and follow-up functions
6. **people**. Introduces the people table of an AMMonitor database.
7. **locations**. Introduces the locations and spatials tables of an AMMonitor database.
8. **temporals**. Introduces the temporals, temporallists, and temporallistitems tables.
9. **equipment**. Introduces the equipment and associated tables in an AMMonitor database.
10. **visits**.  Introduces the visits table in an AMMonitor database.
11.  **mobile_apps**. An introduction to no-code cell phone apps for collecting visit data.
12. **media**. An overview of the AMMonitor media table and media storage options.
13. **taxa**. Introduces the taxa and objectives tables.
14. **annotations**.	Introduces the annotations and associated tables in an AMMonitor database.
15. **dbdistributed**. Introduces and overviews the dbDistributed function
16. **models**. Incorporating models and model outputs into AMMonitor.
17. **ammodels**. Introduces the *AMModels* package for storing ML models.
18. **scripts**. Introduces the scripts table in an AMMonitor database.
19. **sciencebase**. See how to release AMMonitor projects on USGS ScienceBase, and how to re-create a new AMMonitor project from a published release.
20. **amber**. An introduction to the USGS AMBER (**A**lliance for **M**onitoring **B**iodiversity and **E**cosystems **R**emotely). Members of this alliance partner with USGS to use the USGS compute ecosystem (AWS, Posit Connect) to run an AMMonitor project for large monitoring projects.

To run a tutorial, specify the tutorial name, and point to the package *AMMonitor*.  The "intro" tutorial will get you started.

```
learnr::run_tutorial(name = "intro", package = "AMMonitor")
```

The tutorial "getting_started" details all steps needed to start your own AMMonitor project.

```
learnr::run_tutorial(name = "getting_started", package = "AMMonitor")
```

## Contributing

Please join our community and consider submitting issues and/or contributions!

Contributions are welcome from the community. Questions can be asked on the
[issues page][1] or by sending an email to gs_gitlab_servicedesk+vtcfwru-ammonitor-1279-issue-@usgs.gov.  Before creating a new issue, please take a moment to search
and make sure a similar issue does not already exist. If one does exist, you
can comment (most simply even with just a `:+1:`) to show your support for that
issue.

If you have direct contributions you would like considered for incorporation
into the package, you can [fork this repository][2] and
[submit a merge request][3] for review.

## Disclaimer

This software has been approved for release by the U.S. Geological Survey (USGS). Although the software has been subjected to rigorous review, the USGS reserves the right to update the software as needed pursuant to further analysis and review. No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. Furthermore, the software is released on condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from its authorized or unauthorized use.

## Code of Conduct

All contributions to- and interactions surrounding- this project will abide by 
the [USGS Code of Scientific Conduct][4].

[1]: https://code.usgs.gov/vtcfwru/ammonitor/issues
[2]: https://docs.gitlab.com/ee/user/project/repository/forking_workflow.html#project-forking-workflow
[3]: https://code.usgs.gov/vtcfwru/ammonitor/merge_requests
[4]: https://www.usgs.gov/office-of-science-quality-and-integrity/scientific-integrity



