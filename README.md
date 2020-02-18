# trackR
Analyse [trackMate](https://imagej.net/TrackMate) output in R

TrackMate is a kind buddy made by JY Tinevez and helpful for your everyday tracking. TrackMate is available through Fiji. See
Tinevez, JY.; Perry, N. & Schindelin, J. et al. (2017), "TrackMate: An open and extensible platform for single-particle tracking.", Methods 115: 80-90, PMID 27713081

trackR is a R package for reading trackMate xml output files. I implemented basic jump distance analyses (one and two populations) and MSD to characterize diffusion of molecules. It also includes hidden markov models to characterize diffusion regimens amplitudes and dynamics.

Please cite:
Gasser, V.; Malrieu, M.; Forster, A.; Mély, Y.; Schalk, I.J.; Godet, J. (2020), "*In cellulo* FRET-FLIM and single molecule tracking reveal the supra-molecular organization of the pyoverdine bio-synthetic enzymes in *Pseudomonas aeruginosa*."
Q Rev Biophys. 2020 Jan 9;53:e1. doi: [10.1017/S0033583519000155](https://www.cambridge.org/core/journals/quarterly-reviews-of-biophysics/article/in-cellulo-fretflim-and-single-molecule-tracking-reveal-the-supramolecular-organization-of-the-pyoverdine-biosynthetic-enzymes-in-pseudomonas-aeruginosa/0291CA7F55F7443FD15E4C5D53E7D485).


.
