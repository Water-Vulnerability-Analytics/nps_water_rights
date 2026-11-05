# nps_water_rights

This project documents all water rights dockets held for National Park Service (NPS) units in the contiguous United States (CONUS) and maps them, wherever possible, to individual water supply systems in the NPS Water Supply Database (WSD). It identifies water supply systems with missing dockets and defines a workflow for creating those dockets from state-specific water rights portals. In parallel, it identifies all park-held water rights using HarDWR, WestDAAT, and state water rights portals, then develops a plan to update existing dockets and build those that do not exist. Each docket will open with a summary sheet listing all conditions relevant to the day-to-day administration of the right. The project asks whether the NPS water rights inventory is up to date and whether a GIS Shiny application or ArcGIS Experience can be built for this domain. It is intended for the NPS Water Resources Division (WRD), its Water Rights Branch, and park managers.

## Getting Started

1.  Clone this repository:

    ```
    git clone https://github.com/Water-Vulnerability-Analytics/nps_water_rights.git
    ```

2.  Link `data/` to the project's OneDrive data folder from the repository root. The link is ignored by git, so each user must create their own.

    macOS/Linux:

    ```
    ln -s "{path to OneDrive}/WCNR NPS-CSU-DRI - Water for People (WFP)/data" data
    ```

    Windows (Command Prompt as administrator, or with Developer Mode enabled):

    ```
    mklink /D data "{path to OneDrive}\WCNR NPS-CSU-DRI - Water for People (WFP)\data"
    ```

3.  Open `nps_water_rights.Rproj` in RStudio.

4.  Run the setup script to install and load packages and source all functions:

    ```r
    source("setup.R")
    ```

5.  {Python version, packages, and Tesseract installation steps for the PDF conversion scripts}

## Folder Descriptions (general)

-   **{OneDrive}** `data/` Project data, including offline water rights dockets (PDFs), the Water Supply Database, and docket-to-WSD matching outputs. Not tracked by git.

-   `Processing/` Scripts used to process data for project workflows. {planned}

-   `scratch/` Miscellaneous scripts that are in development or exploratory in nature. {planned}

-   `src/` Functions used in project workflows. This folder began as a copy of `src/` from [nps_water_vulnerability](https://github.com/Water-Vulnerability-Analytics/nps_water_vulnerability), is maintained separately, and will be trimmed to the functions this project needs. State-specific water rights functions are in state folders (e.g., `src/CO/`).

    [Shiny Applications:]{.underline}

-   `{app folder}/` {GIS Shiny application or ArcGIS Experience, pending feasibility assessment}

## File Descriptions

There are a few important files in the root directory:

-   `setup.R` Executing this script will install and load all necessary packages and source all functions.

-   `nps_water_rights.Rproj` RStudio project file. Open this first so that relative paths resolve from the repository root.

## Workflows

1.  **Inventory existing dockets.** Dockets listed in `DocketList.csv` are downloaded from the IRMA DataStore API. {in progress}

2.  **Convert imaged PDFs to markdown.** Imaged dockets are converted to markdown using OCR (Tesseract). {in progress}

3.  **Map dockets to water supply systems and flag gaps.** Dockets are matched to WSD records retrieved with `get_wsd()`, and systems without dockets are identified. {in progress}

4.  **Compile park-held water rights.** Rights are compiled from HarDWR, WestDAAT, and state water rights portals. {approach for states not covered by HarDWR or WestDAAT: undecided}

5.  **Plan docket updates and new dockets.** Develop a plan to update existing dockets and define a state-specific workflow for creating missing ones. {under development}

6.  **Build docket summary sheets.** Add a front summary sheet to each docket listing all conditions for day-to-day administration of the right. {under development}

7.  **Assess a GIS application.** Determine whether a GIS Shiny application or ArcGIS Experience can be built for this domain. {under development}

### Docket Pipeline

Below is the current file structure for the docket pipeline. Existing download and conversion scripts will be moved and developed into `src/get_waterrightsdockets()`. {under development}

```
data/
├── all/
│   └── NPS_water_rights_dockets/
│       ├── DocketList.csv
│       ├── Dockets/
│       └── download_dockets.R
└── Water_Supply_Systems/
    ├── NPS_Water_Systems_Database_Joined.xlsx
    └── docket_link/
        ├── all_dockets.csv
        ├── docket_matches_review.csv
        ├── ALL_PARKS_docket_match_summary.xlsx
        ├── {PARK}_docket_matches_proposed.csv
        ├── {PARK}_docket_matches_proposed.xlsx
        ├── docket_markdown/
        │   ├── _manifest.csv
        │   ├── _small_parks/
        │   └── {PARK}/
        └── scripts/
```

`DocketList.csv`: List of dockets and their IRMA DataStore reference codes.

`download_dockets.R`: Downloads the dockets in `DocketList.csv` from the IRMA DataStore API, skips files already downloaded, and writes a download log.

`get_wsd()`: Loads and cleans the NPS Water Supply Database, with options to filter by CONUS and park unit.

`docket_link/scripts/`: {confirm each description}

-   `download_missing_dockets.py` {downloads dockets missing from the local collection}
-   `build_docket_manifest.py` {builds the docket manifest}
-   `convert_dockets_to_md.py` {converts docket PDFs to markdown}
-   `extract_docket_evidence.py` {extracts evidence from docket text for matching}
-   `build_docket_review.py` {builds the docket match review tables}
-   `review_packet.py` {creates review packets}
-   `small_parks_first_pass.py` {runs a first matching pass for small parks}

## Acknowledgements

Collaborators: {names and affiliations}

Funding: {funder and agreement number}

Partners: {partner organizations}
