---
title: "Installation"
---
# Installation

## OS-specific instructions {.tabset}

### Windows

#### Installing on Windows

<span style="font-size: 1.1em;"><b>Prerequisites</b></span>

<b> 1) Install R and the recommended interface RStudio, if not already installed </b>

Follow the instructions on the [CRAN](https://cran.r-project.org/) (for R) and [Posit](https://posit.co/downloads/) for (for RStudio) for Windows.

<b> 2) Install Microsoft C++ build tools, if not already installed </b> <br>
i) Download and run the installer from: https://visualstudio.microsoft.com/visual-cpp-build-tools/  
ii) During installation, check:
   - “Desktop development with C++” or “C++ build tools”.
   - Ensure “Windows 11 SDK” is also selected on the right menu. <br>
iii) Complete installation and restart your computer.

<span style="font-size: 1.1em;"><b>Install and set up the `text` package</b></span>

The text package requires a working Python environment, which can be set up directly from R. First install the text package in R, then configure it to install and use the required Python dependencies. During installation, you may see messages about additional, OS-specific system dependencies that need to be installed; see the sections below for more detailed instructions.
install.packages("text")
  
    install.packages("text")

    # Install text required python packages in a conda environment (with defaults).
    text::textrpp_install()

    # Initialize the installed conda environment.
    text::textrpp_initialize(save_profile = TRUE)

---

### MacOS

#### Installing on MacOS

<span style="font-size: 1.1em;"><b>Prerequisites</b></span>

<b> 1) Install R and the recommended interface RStudio for macOS, if not already installed </b>

Follow the instructions on the [CRAN](https://cran.r-project.org/) (for R) and [Posit](https://posit.co/downloads/) for (for RStudio) for Mac.
Remember to use arm64 if you have Apple’s OS family (e.g., M1, M2, M3) and x86_64 if you have an older Intel OS.


<b> 2) Install Homebrew and libomp in the terminal (not in R), if not already installed </b>

For Homebrew, run (in terminal)

    /bin/bash -c "$(curl -fsSLhttps://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

For libomp, run (in terminal)

    brew install libomp

<span style="font-size: 1.1em;"><b>Install and set up the `text` package</b></span>

The text package requires a working Python environment, which can be set up directly from R. First install the text package in R, then configure it to install and use the required Python dependencies. During installation, you may see messages about additional, OS-specific system dependencies that need to be installed; see the sections below for more detailed instructions.
install.packages("text")
  
    install.packages("text")

    # Install text required python packages in a conda environment (with defaults).
    text::textrpp_install()

    # Initialize the installed conda environment.
    text::textrpp_initialize(save_profile = TRUE)

---


### Linux

#### Installing on Linux

<span style="font-size: 1.1em;"><b>Prerequisites</b></span>

<b>1) Install R and the recommended interface RStudio, if not already installed </b>

Follow the instructions on the [CRAN](https://cran.r-project.org/) (for R) and [Posit](https://posit.co/downloads/) for (for RStudio) for your Linux distribution.

<span style="font-size: 1.1em;"><b>Install and set up the `text` package</b></span>

The text package requires a working Python environment, which can be set up directly from R. First install the text package in R, then configure it to install and use the required Python dependencies. During installation, you may see messages about additional, OS-specific system dependencies that need to be installed; see the sections below for more detailed instructions.
install.packages("text")
  
    install.packages("text")

    # Install text required python packages in a conda environment (with defaults).
    text::textrpp_install()

    # Initialize the installed conda environment.
    text::textrpp_initialize(save_profile = TRUE)


<b> Common pitfall </b> <br>
On recent Ubuntu distributions (e.g., 22.04+), most core dependencies are available.  
If needed, you can install them with (run this in your **Terminal**, not in R):

    sudo apt update
    sudo apt install build-essential libomp-dev

- `build-essential`: provides gcc, g++, and make  
- `libomp-dev`: for OpenMP support  

---



### Troubleshooting

<b> 1. Check if you have install permissions </b>

Can you install an R package like **dplyr**?

    install.packages("dplyr")

Can you install system-level tools like Python / Miniconda?

    library(reticulate)
    reticulate::install_miniconda()

If you do not have permissions, please contact your administrator for advice.

<b> 2. Remember to initialize the Python environment </b>

After restarting R, functions like `textEmbed()` can stop working again.

Solution: persist the initialization in your R profile:

    text::textrpp_initialize(
      condaenv = "textrpp_condaenv",
      refresh_settings = TRUE,
      save_profile = TRUE
    )

<b>3. Install the development version from GitHub </b>

    # install.packages("devtools")
    devtools::install_github("oscarkjell/text")

<b> 4. Force reinstallation of the environment </b>

    library(text)
    text::textrpp_install(
      update_conda = TRUE,
      force_conda  = TRUE
    )

<b>5. Install the Python environment using `reticulate` </b>

See the article [Installing and Managing Python Environments with reticulate](https://r-text.org/articles/reticulate.html) for detailed information.

<b> 6. Inspect diagnostic information </b>

If something isn’t working right, it is a good start to examine what is installed and running on your system.

    library(text)
    log <- text::textDiagnostics()
    log

Because the **text** package requires some system-level setup, installation is automatically verified on Windows, macOS, and Ubuntu through our GitHub Actions. If you encounter any issues, please review the tests and check the workflow file for details on system-specific installations.

To view the workflow file, select the three-dot menu on the right side of any GitHub Action run and choose [View workflow file](https://github.com/OscarKjell/text/actions). This file specifies the operating systems, R versions, and additional libraries being tested.


<b> 7. GitHub Issues </b>
<br>
First check out [closed GitHub issues](https://github.com/OscarKjell/text/issues?q=is%3Aissue%20state%3Aclosed), and if you cannot find your problem being solved, please open a new issue.
<br>
If you run into issues that aren’t covered here, please reach out to us at:

    rtext.contact@gmail.com

so that we can improve the instructions for everyone.



```{=html}
<script>
document.addEventListener("DOMContentLoaded", function() {
  // Find the first tabset on the page (your OS-specific tabset)
  var tabset = document.querySelector(".tabset, .panel-tabset");
  if (!tabset) return;

  // All tab headers
  var navLinks = tabset.querySelectorAll("ul.nav-tabs a, .nav.nav-tabs .nav-link");
  // All tab panes
  var panes = tabset.querySelectorAll(".tab-content .tab-pane, .tab-content > .tab-pane");

  if (navLinks.length === 0 || panes.length === 0) return;

  // Remove 'active' from every tab header
  navLinks.forEach(function(link) {
    link.classList.remove("active");
    link.setAttribute("aria-selected", "false");
  });

  // Hide every tab pane
  panes.forEach(function(pane) {
    pane.classList.remove("active", "show");
  });
});

// Extra styling: separate the last tab (Troubleshooting) visually
document.addEventListener("DOMContentLoaded", function() {
  var lastTabLink = document.querySelector(".tabset .nav-tabs .nav-item:last-child .nav-link");
  if (!lastTabLink) return;
  lastTabLink.style.marginLeft = "2rem";
  lastTabLink.style.borderLeft = "1px solid #ddd";
  lastTabLink.style.paddingLeft = "1rem";
});
</script>
```
