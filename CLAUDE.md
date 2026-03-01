# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Academic course repository for **Applied Statistics II (POU7003)** at Trinity College Dublin, Spring 2026. Focuses on Generalized Linear Models (GLMs) and Maximum Likelihood Estimation (MLE) in R. Forked from the upstream course repo at `ASDS-TCD/StatsII_2026`.

## Repository Structure

- **tutorials/** — Weekly R tutorial scripts (scaffolded exercises with paired solution files)
- **problemSets/** — Graded assignments with templates (`template/`), answer keys (`answer_key/`), and student work (`my_answers/`)
- **datasets/** — Shared CSV, RData, and text data files used across tutorials and problem sets

## Working with R Scripts

All code is written in R (version 4+), intended to be run interactively in RStudio.

Scripts follow a consistent pattern:
1. Clean environment with `rm(list=ls())` and a custom `detachAllPackages()` helper
2. Install/load packages via a custom `pkgTest()` function
3. Set working directory with `setwd(dirname(rstudioapi::getActiveDocumentContext()$path))`
4. Load data, perform analysis, and generate output

Key libraries: `tidyverse`, `ggplot2`, `stargazer` (for LaTeX regression tables).

There is no build system, test suite, or linter. Scripts are run manually in RStudio.

## LaTeX Integration

Problem sets use LaTeX (`.tex` → `.pdf`) for write-ups. `stargazer()` generates LaTeX-formatted regression tables that get pasted into `.tex` templates. Plots are saved via `ggsave()` or RStudio's export and included in LaTeX documents.

## Git Workflow

This is a student fork. The upstream course repo is `ASDS-TCD/StatsII_2026`. Student work goes in `problemSets/PS*/my_answers/` and tutorial solution files in `tutorials/Week */`.
