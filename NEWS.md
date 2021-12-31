# 2021-12-31

- Updated the summary text to reflect post-Christmas changes.
- Updated the time at 95% Omicron plot to include median estimates. This was needed to prevent estimates disappearing when uncertainty was less than or equal to a day.

# 2021-12-29

- Fixed a bug for the centred 7-day moving average where partially complete case data was included for regions. This impacted on the data overview plot.

# 2021-12-28

- Updated data and results archiving to be by date of access rather than by the maximum date available in the data. This has the download of making an update occur every day the pipeline runs regardless of if the data has been updated but the upside of protecting archived data from retrospective updates.

- Updates dating of all archived data and results to be by date of access rather than by maximum available date in the data.

# 2021-12-26

- Automation has been improved so that new data is checked for every two hours. A new model run is only started if not result exist for the current data snapshot or if new SGTF data is detected.

- Updated the plotting of SGTF case and target failure data to be more flexible for other analyses.


# 2021-12-23

- Thanks to [Ritwik Priya](https://twitter.com/ritwik_priya) for spotting an issue with our cumulative incidence as a percentage of the population estimates. Unfortunately, our code accidently combined all cases types (Omicron, non-Omicron, and combined) when making the calculating leading to estimates roughly 3 times higher than they should have been.
- Update the region order of the cumulative incidence plot to match other regions.

# 2021-12-22

- Initial analysis release.