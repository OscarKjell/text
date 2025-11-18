---
title: "Installation"
---

# Installation

Select your operating system below to see installation instructions for the **text** package and its Python backend.  
The final tab contains general troubleshooting tips.

## Operating-system-specific instructions {.tabset}

### Linux

#### Installing on Linux

##### Prerequisites

Install R (and RStudio).  
Use the links below to download and install:

- R (version 4.0.0 or higher)  
- RStudio (recommended)

R for Linux: https://cran.r-project.org/  
Click on “Download R for Linux”.  
RStudio: https://posit.co/download/rstudio-desktop/

##### 1) Install R (and RStudio)

Follow the instructions on the CRAN and Posit (RStudio) pages for your Linux distribution.

##### 2) Install and set up the `text` package

The **text** package gives you access to HuggingFace Transformers through **reticulate** (enabling advanced language analysis), which connects R to Python (while remaining in an R environment). It uses Python packages like **torch** and **transformers**.

To make this easy, run:

- `textrpp_install()` – installs a ready-to-use Python/Conda environment  
- `textrpp_initialize()` – activates the environment for use in R  

This setup will handle most Python and system dependencies automatically – however, you may be instructed to install system-level dependencies as further described below.

    install.packages("text")
    library(text)

    # Install text required python packages in a conda environment (with defaults).
    textrpp_install()

    # Initialize the installed conda environment.
    # save_profile = TRUE saves the settings so that you don't have to run
    # textrpp_initialize() after restarting R.
    textrpp_initialize(save_profile = TRUE)

On recent Ubuntu distributions (e.g., 22.04+), most core dependencies are available.  
If needed, you can install them with (run this in your **Terminal**, not in R):

    sudo apt update
    sudo apt install build-essential libomp-dev

- `build-essential`: provides gcc, g++, and make  
- `libomp-dev`: for OpenMP support  

---

### macOS

#### Installing on Mac

##### Prerequisites

R for mac: https://cran.r-project.org/bin/macosx/

- If arm64: click on `R-x.x.x-arm64.pkg`  
- If x86_64: click on `R-x.x.x-x86_64.pkg`

RStudio: https://posit.co/download/rstudio-desktop/  
Homebrew  
`libomp`

##### Installation

    install.packages("text")
    library(text)

    # Install text required python packages in a conda environment (with defaults).
    textrpp_install()

    # Initialize the installed conda environment.
    # save_profile = TRUE saves the settings so that you don't have to run
    # textrpp_initialize() after restarting R.
    textrpp_initialize(save_profile = TRUE)

On macOS, most system-level dependencies are typically pre-installed. For any missing components, the **text** package automatically detects them and provides clear instructions to guide the user through installation.

##### 1) Homebrew & libomp

Install Homebrew (if not already installed). Run this in your **Terminal** (not in R):

    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

2) Install `libomp` (also in Terminal, not in R):

    brew install libomp

---

### Windows

#### Installing on Windows

##### Prerequisites

- R for Windows: https://cran.r-project.org/bin/windows/base/  
- RStudio: https://posit.co/download/rstudio-desktop/  
- Microsoft C++ build tools  

##### Installation

    install.packages("text")
    library(text)

    # Install text required python packages in a conda environment (with defaults).
    textrpp_install()

    # Initialize the installed conda environment.
    # save_profile = TRUE saves the settings so that you don't have to run
    # textrpp_initialize() after restarting R.
    textrpp_initialize(save_profile = TRUE)

##### Microsoft C++ Build Tools

Some Python packages (e.g., `hdbscan`, `flair`) require compilation with Visual C++. Windows users may need to manually install Microsoft C++ Build Tools. You must have C++ build tools for Python packages that need compilation.

Install the latest version of Microsoft C++ Build Tools:

1. Download and run the installer from: https://visualstudio.microsoft.com/visual-cpp-build-tools/  
2. During installation, check:
   - “Desktop development with C++” or “C++ build tools”.
   - Ensure “Windows 11 SDK” is also selected on the right menu.
3. Complete installation and restart your computer.

##### Terms of Service conflict – Anaconda / conda-forge channel

Python environments used by the **text** and **talk** packages rely on packages from the **conda-forge** channel.

You don’t need to install Anaconda manually – the `textrpp_install()` function will install Miniconda and set `conda-forge` as the default channel.

However, if you’ve previously installed Anaconda or changed channels, you might encounter ToS (Terms of Service) conflicts.

If errors mention `pkgs/main` or `tos accept`, you can fix it by running the following in **Terminal / Command Prompt** (not in R):

    conda config --add channels conda-forge
    conda config --set channel_priority strict
    conda config --remove channels defaults

---

### Troubleshooting

#### 1. Check if you have install permissions

Can you install an R package like **dplyr**?

    install.packages("dplyr")

Can you install system-level tools like Python / Miniconda?

    library(reticulate)
    reticulate::install_miniconda()

If you do not have permissions, please contact your administrator for advice.

#### 2. Remember to initialize the Python environment

After restarting R, functions like `textEmbed()` can stop working again.

Solution: persist the initialization in your R profile:

    text::textrpp_initialize(
      condaenv = "textrpp_condaenv",
      refresh_settings = TRUE,
      save_profile = TRUE
    )

#### 3. Install the development version from GitHub

    # install.packages("devtools")
    devtools::install_github("oscarkjell/text")

#### 4. Force reinstallation of the environment

    library(text)
    text::textrpp_install(
      update_conda = TRUE,
      force_conda  = TRUE
    )

#### 5. Install the Python environment using reticulate

See the article *Installing and Managing Python Environments with reticulate* for detailed information.

#### 6. Inspect diagnostic information

If something isn’t working right, it is a good start to examine what is installed and running on your system.

    library(text)
    log <- text::textDiagnostics()
    log

Because the **text** package requires some system-level setup, installation is automatically verified on Windows, macOS, and Ubuntu through our GitHub Actions. If you encounter any issues, please review the tests and check the workflow file for details on system-specific installations.

To view the workflow file, select the three-dot menu on the right side of any GitHub Action run and choose **“View workflow file”**. This file specifies the operating systems, R versions, and additional libraries being tested.

If you run into issues that aren’t covered here, please reach out to us at:

    rtext.contact@gmail.com

so that we can improve the instructions for everyone.
