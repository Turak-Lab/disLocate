
**disLocate** (**D**etecting **I**ntermolecular **S**tructure **Locate**d at particle positions) is a Mathematica package that calculates various translational, entropic and angular order metrics and parameters from a list of particle centroid positions. These are displayed in various configurations to give access to the  spatial statistical characteristics of 2D arrays of objects.  
See M. Bumstead, K. Liang, G. Hanta, L.S. Hui, and A. Turak, “disLocate: tools to rapidly quantify local intermolecular structure to assess two-dimensional order in self-assembled systems,” Scientific Reports **8**(1), 1554 (2018).Primarily used for analyzing arrays of nanoparticles from AFM and SEM images, it can be easily applied to any 2D array of objects (eg. organization of opals and inverse opals as photonic crystals, the distribution of self-trapping filaments in polymer waveguides, the distribution of dopants for OLEDs, and simulated molecules). Notable references include:

1. S.I. Lee, M. Munir, R. Arbi, P. Oliveira, S.J. Lee, J.H. Lim, W.Y. Kim, and A. Turak, “Uncoupling nanoparticle geometry from material properties for improved hole injection at submonolayer nanoparticle electrode interlayers in organic hole-only devices,” J Mater Sci: Mater Electron **34**(13), 1101 (2023).
2. S.A. Estrada, S. Gaidies, J. Febbraro, A. Turak, H.-R. Lin, Y. Salinas, and O. Brüggemann, “Spatial characterization of peptide nucleic acid molecularly imprinted inverse opal,” Monatsh Chem, (2023).
3. S.A. Estrada Alvarez, I. Guger, J. Febbraro, A. Turak, H.-R. Lin, Y. Salinas, and O. Brüggemann, “Synthesis and Spatial Order Characterization of Controlled Silica Particle Sizes Organized as Photonic Crystals Arrays,” Materials **15**(17), 5864 (2022).
4. T. Tokubuchi, R.I. Arbi, P. Zhenhua, K. Katayama, A. Turak, and W.Y. Sohn, “Enhanced photoelectrochemical water splitting efficiency of hematite (α-Fe2O3)-Based photoelectrode by the introduction of maghemite (γ-Fe2O3) nanoparticles,” J. Photochem. Photobiol. A **410**, 113179 (2021).
5. M. Bumstead, B. Arnold, and A. Turak, “Reproducing morphologies of disorderly self-assembling planar molecules with static and dynamic simulation methods by matching density,” Physica A: Statistical Mechanics and Its Applications **471**, 301–314 (2017).


Provided by Turak Lab

## Installation
 - **For Mathematica 12.1 and above** [download](https://github.com/MstislavKeldysh/disLocate/tree/main) the latest version of the .paclet file and save it in some known directory.
 - Open an empty notebook, place and run the following code in a cell
	 ```
	 Needs["PacletManager`"]
	 PacletInstall["Path to the paclet file here"]
	```

- **To use** disLocate for your analysis, simply place the following at the beginning of your notebook
```
Needs["disLocate`"]
```

- To **Uninstall** simply run ```PacletUninstall["disLocate`"]  ```

- For **Mathematica 10.0** and above, check out the options in the Installation tutorial file (in /Tutorials).

## Using disLocate 
- With disLocate installed as a paclet, you can access all of its content after running ```Needs["disLocate`"]```
- If you have not used Mathematica before, you can read our the 0.Basics of Mathematica file in /Tutorials. 
- Once installed, you can access the documentation files in the Wolfram Documentation centre, by searching "disLocate".
- The ``/Example`` folder contains a sample analysis of two datasets. Simply download the entire folder and open the "Analysis.nb" file.
## About Us
  
The Turak Functional Nanomaterials Research Group at Concordia University (Physics, Centre for NanoScience Research) aims to develop easy, versatile, and inexpensive methods of producing, tuning and exploring nanostructures, targeting energy applications, sensing and magneto-optics. By making cheaper, more accessible, and more flexible products, our research makes an impact on how people use clean energy, access information and measure the world around them.
