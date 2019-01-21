pro oxford_to_mca, infile, outfile
;+
; NAME:
;       OXFORD_TO_MCA
;
; PURPOSE:
;       This procedure reads a spectra file in Oxford ASCII (.asc) format 
;       and writes it out in our ASCII MCA format.
;
; CATEGORY:
;       MCA software
;
; CALLING SEQUENCE:
;
;       OXFORD_TO_MCA, Infile, Outfile
;
; INPUTS:
;       Infile  The name of the input file in Oxford's format
;       Outfile The name of the output file in our format
;
; PROCEDURE:
;       This procedure creates an object of type MCA, reads the input file, 
;       and copies the information to the MCA object.  It then invokes
;       MCA::WRITE_FILE to write the data in our format.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, November 19, 2001
;-

    mca = obj_new('mca')
    elapsed = mca->get_elapsed()
    cal = mca->get_calibration()
    openr, lun, /get, infile
    line = ""

    ; Skip first 5 lines
    for i=1, 5 do readf, lun, line

    ; Acquisition start time
    readf, lun, line
    pos = strpos(line, ':')
    elapsed.start_time = strtrim(strmid(line, pos+1, 1000), 2)

    ; Elapsed real time
    readf, lun, line
    pos = strpos(line, ':')
    elapsed.real_time = float(strmid(line, pos+1, 1000))

    ; Elapsed live time
    readf, lun, line
    pos = strpos(line, ':')
    elapsed.live_time = float(strmid(line, pos+1, 20))

    ; Number of channels
    readf, lun, line
    pos = strpos(line, ':')
    nchans = long(strmid(line, pos+1, 1000))

    ; Calibration offset
    readf, lun, line
    pos = strpos(line, '=')
    cal.offset = float(strmid(line, pos+1, 1000))

    ; Calibration slope
    readf, lun, line
    pos = strpos(line, '=')
    cal.slope = float(strmid(line, pos+1, 1000))

    ; Calibration quad
    readf, lun, line
    pos = strpos(line, '=')
    cal.quad = float(strmid(line, pos+1, 1000))

    ; Skip 3 lines
    for i=1, 3 do readf, lun, line

    ; Read data
    data = lonarr(2,nchans)
    readf, lun, data
    data = reform(data[1,*])
    
    ; Close input file
    free_lun, lun

    ; Copy information to mca object, write output file
    mca->set_elapsed, elapsed
    mca->set_calibration, cal
    mca->set_data, data
    mca->write_file, outfile
end
