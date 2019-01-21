; This script makes mca.sav, to run under IDL Virtual Machine
.compile mca
.compile mca__define
.compile epics_mca__define
.compile hardware_mca__define
.compile med__define
.compile epics_med__define
.compile mca_display__define
.compile mca_peak__define
.compile mca_background__define
.compile mca_fit__define
.compile jcpds__define
resolve_all
save, /routine, file='mca.sav'
