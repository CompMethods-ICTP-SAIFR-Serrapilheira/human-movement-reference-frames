# Comparison between inertial and non-inertial referential frames in human reaching movements

Serrapilheira/ICTP-SAIFR Training Program in Quantitative Biology and Ecology

**Author: Mateus Souza Silva** 

## Background

The referential of the coordinate system used by the motor control is a point under discussion until today in the biomechanical field. 
There are authors who consider the referential as inertial, but another system widely used in the literature is the joint coordinative system (JCS), 
which corresponds to a non-inertial referential in which the movement of a joint is planned based on the previous joint.

## Goal

In order to verify which of these two referential better corresponds to the referential used by the motor control during
the motion planning process, we will use the decomposition method into motion elements and compare it with the theoretical jerk minimization curve.

## Dataset

The data used in this project was provided by Liang et al. (2020) <https://doi.org/10.1038/s41597-020-00627-7>
<br>
The raw data was organized into 10 folders, one for each individual. Each individual performed a set of collections with different tasks that are divided into different files.
<br>
To perform the analyses in R it was necessary to convert the files from ```.c3d``` to ```.csv``` using a function I developed called ```c3d_to_csv.m```, located in the folder ```fct```.

## Methods

In this project we use the movement element decomposition to enable comparison of the complex motion with the theoretical velocity profile related to the minimum jerk optimization. 
<br>
For more details check the file ```Report_human_movement_referentials.pdf``` in the ```docs``` folder.

## Project Structure

```
Project/
     ├── data/
     │   ├── raw
     |   |   ├── c3d
     |   |   └── csv
     │   └── processed
     |       └── filtered_data
     |           ├── inertial
     |           └── non_inertial
     ├── docs/
     ├── fct/
     ├── figures/
     ├── output/
     ├── R/
     └── README.md
```

Inside ```docs``` you will find the ```.pdf``` report and its references in a ```.bib``` file


## Requirements

```
R libraries:
- dplyr
- ggplot2
- lme4
- pracma
- stringi
- signal
```
