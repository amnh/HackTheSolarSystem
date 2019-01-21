;*****************************************************************************
;+
; NAME:
; Release notes
;   Version 3:
;       Everything prior to 3/17/99 when I started making release notes!
;       Prior to this date there are release notes for each routine but no
;       global release notes for the EPICS_MCA class, and there are no
;       version numbers
;   Version 4.0
;       March 17, 1999.  Mark Rivers.
;           Removed the /WAIT qualifier from all CAPUT calls to make the class
;           compatible with Version 4.3 of the EPICS MCA record.
;           Added monitors to all PVs, including .VAL field
;           Changed logic of getting acquisition start time from using
;           catimestamp (Ben-Chin's routine) to using the new .STIM field in
;           the record.  This requires version 4.6 or greater of the record.
;       April 15, 1999. Mark Rivers
;           Added CHECK_NEW and NEW_FLAG to GET_ELAPSED, GET_DATA and
;           GET_ACQUIRE_STATUS.
;  Version 4.1
;       May 16, 1999  Mark Rivers
;           Added .read_time field to MCA_ELAPSED and put a monitor on it for
;           accurate counts/second calculations
;  Version 4.2
;       Dec. 1, 1999 Mark Rivers
;           Fixed bug in epics_mca::init when reading environment file.
;       Dec. 2, 1999 Mark Rivers
;           Added .dwell, .channel_advance and .prescale to MCA_PRESETS
;       Dec. 3, 1999 Mark Rivers
;           Changed GET_DATA to read .NUSE each time, adjust self.nchans.
;           This permits NUSE to be changed after the EPICS_MCA is created.
;           Modified WRITE_FILE to reset ClientWait after file is written.
;       April 18, 2001 Mark Rivers
;           Added MCA::SPECTRA_SCAN to collect spectra using the "ClientWait"
;           PV.  This already existed in EPICS_MED, added to EPICS_MCA.
;           Added the /START and /STOP keywords to EPICS_MCA::ACQUIRE_WAIT
;       Sept. 28, 2001 Mark Rivers
;           Changed GET_ROIS to use caGetArray and SET_ROIS to use caPutArray.
;           This speeded up GET_ROIS by a factor of 32 and SET_ROIS by a factor
;           of 32 the first time, not much on subsequent calls.
;       Sept. 29, 2001 Mark Rivers
;           Added GET_ENVIRONMENT.  This code used to be in MCA::WRITE_FILE
;           where it did not belong.
;           Deleted calls to read the data, rois, etc. in EPICS_MCA::
;           WRITE_FILE, since this was done again in MCA::WRITE_FILE.
;           Changed many calls from caGet to caGetArray.  This eliminates the
;           need for caSetMonitor which was slowing things down at startup.
;       March 5, 2002 Mark Rivers
;           Changed acquire_wait to poll every dwell_time/100. from dwell_time/10.
;           to reduce latencies
;-
;

;*****************************************************************************
function epics_mca::get_calibration
;+
; NAME:
;       EPICS_MCA::GET_CALIBRATION
;
; PURPOSE:
;       This function returns the calibration parameters for the MCA.
;       The calibration information is contained in a structure of type
;       MCA_CALIBRATION.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;
;       Result = epics_mca->GET_CALIBRATION()
;
; PROCEDURE:
;       This function reads the calibration information from the hardware using
;       the EPICS MCA record, and then invokes MCA::GET_CALIBRATION
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::GET_CALIBRATION">MCA::GET_CALIBRATION()</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;       19-Sep-1998 MLR  Added caStartGroup, caEndGroup for efficiency
;-
    pvs = strarr(5)
    svals = ''
    dvals = dblarr(4)
; 
    ; Must fetch numbers as doubles, not strings, because of precision
    pvs[0]=self.record_name+'.CALO'
    pvs[1]=self.record_name+'.CALS'
    pvs[2]=self.record_name+'.CALQ'
    pvs[3]=self.record_name+'.TTH'
    pvs[4]=self.record_name+'.EGU'
    t = caGetArray(pvs[0:3], dvals)
    t = caGetArray(pvs[4], svals, /string)
    self.calibration.offset = dvals[0]
    self.calibration.slope = dvals[1]
    self.calibration.quad = dvals[2]
    self.calibration.two_theta = dvals[3]
    self.calibration.units = svals[0]
    return, self->mca::get_calibration()
end


;*****************************************************************************
pro epics_mca::set_calibration, calibration
;+
; NAME:
;       EPICS_MCA::SET_CALIBRATION
;
; PURPOSE:
;       This procedure sets the calibration parameters for the MCA.
;       The calibration information is contained in a structure of type
;       MCA_CALIBRATION.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;
;       epics_mca->SET_CALIBRATION, Calibration
;
; PROCEDURE:
;       This function invokes MCA::SET_CALIBRATION and then writes the
;       calibration information to the hardware using the EPICS MCA record.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::SET_CALIBRATION">MCA::SET_CALIBRATION</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;       17-Sep-1998 MLR  Removed caStartGroup, caEndGroup, since these don't
;                        work with the non-callback version of caput, which
;                        we are now using (ezcaPutOldCa)
;-
    self->mca::set_calibration, calibration
    t = caput(self.record_name+'.CALO', self.calibration.offset)
    t = caput(self.record_name+'.CALS', self.calibration.slope)
    t = caput(self.record_name+'.CALQ', self.calibration.quad)
    t = caput(self.record_name+'.EGU',  self.calibration.units)
    t = caput(self.record_name+'.TTH',  self.calibration.two_theta)
end


;*****************************************************************************
function epics_mca::get_elapsed, new_flag, check_new=check_new
;+
; NAME:
;       EPICS_MCA::GET_ELAPSED
;
; PURPOSE:
;       This function returns the elapsed parameters for the MCA.
;       The elapsed information is contained in a structure of type
;       MCA_ELAPSED.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;       Result = epics_mca->GET_ELAPSED()
;
; KEYWORD_PARAMETERS:
;       CHECK_NEW:
;           A flag which indicates that this routine should only return
;           the elapsed parameters if they have changed.
;
; OPTIONAL OUTPUTS:
;       NEW_FLAG:
;           If CHECK_FLAG is set, then NEW_FLAG will be 1 if the function
;           is returning new elapsed parameters, 0 if the function is not
;           returning new elapsed parameters.  If CHECK_FLAG is set and
;           NEW_FLAG is 0 then the function returns -1.
;
; PROCEDURE:
;       This function reads the elapsed information from the hardware using
;       the EPICS MCA record, and then invokes MCA::GET_ELAPSED
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::GET_ELAPSED">MCA::GET_ELAPSED()</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;       May 5, 1998:    Mark Rivers, added reading the acquisition start time
;                       from the timestamp field of the record.
;       19-Sep-1998 MLR Added caStartGroup, caEndGroup for efficiency
;       15-APR-1999 MLR Changed from using catimestamp to reading the .STIM
;                       field of the record
;       16-MAY-1999 MLR Added .read_time field
;       28-SEP-2001 MLR Added caStartGroup and caEndGroup
;-

    if keyword_set(CHECK_NEW) then begin
        new_flag = cacheckmonitor(self.record_name + '.ERTM')
        if (new_flag eq 0) then return, -1
    endif
    svals = ''
    dvals = dblarr(5)
; 
    pvs = strarr(5)
    pvs[0]=self.record_name+'.ERTM'
    pvs[1]=self.record_name+'.ELTM'
    pvs[2]=self.record_name+'.ACT'
    pvs[3]=self.record_name+'.RTIM'
    pvs[4]=self.record_name+'.STIM'

    t=caGetArray(pvs[0:3], dvals, /double)
;
    self.elapsed.real_time    = dvals[0]
    self.elapsed.live_time    = dvals[1]
    self.elapsed.total_counts = dvals[2]
    self.elapsed.read_time    = dvals[3]
;
    t=caGetArray(pvs[4], svals, /string)
    if (t eq 0) then self.elapsed.start_time = strtrim(svals[0],2)
    return, self->mca::get_elapsed()
end


;*****************************************************************************
function epics_mca::get_presets
;+
; NAME:
;       EPICS_MCA::GET_PRESETS
;
; PURPOSE:
;       This function returns the preset parameters for the MCA.
;       The preset information is contained in a structure of type
;       MCA_PRESETS.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;
;       Result = epics_mca->GET_PRESETS()
;
; PROCEDURE:
;       This function reads the preset information from the hardware using
;       the EPICS MCA record, and then invokes MCA::GET_PRESETS
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::GET_PRESETS">MCA::GET_PRESETS()</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;       19-Sep-1998 MLR  Added caStartGroup, caEndGroup for efficiency
;-
    pvs = strarr(8)
    vals = dblarr(8)
    pvs[0]=self.record_name + '.PRTM'
    pvs[1]=self.record_name + '.PLTM'
    pvs[2]=self.record_name + '.PCT'
    pvs[3]=self.record_name + '.PCTL'
    pvs[4]=self.record_name + '.PCTH'
    pvs[5]=self.record_name + '.DWEL'
    pvs[6]=self.record_name + '.CHAS'
    pvs[7]=self.record_name + '.PSCL'
    t = caGetArray(pvs, vals, /double)
    self.presets.real_time = vals[0]
    self.presets.live_time = vals[1]
    self.presets.total_counts = vals[2]
    self.presets.start_channel = vals[3]
    self.presets.end_channel = vals[4]
    self.presets.dwell = vals[5]
    self.presets.channel_advance = vals[6]
    self.presets.prescale = vals[7]
    return, self->mca::get_presets()
end


;*****************************************************************************
pro epics_mca::set_presets, presets
;+
; NAME:
;      EPICS_MCA::SET_PRESETS
;
; PURPOSE:
;       This procedure sets the preset parameters for the MCA.
;       The preset information is contained in a structure of type
;       MCA_PRESETS.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;
;       epics_mca->SET_PRESETS, Presets
;
; PROCEDURE:
;       This function invokes MCA::SET_PRESETS and then writes the
;       preset information to the hardware using the EPICS MCA record.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::SET_PRESETS">MCA::SET_PRESETS</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;       17-Sep-1998 MLR  Removed caStartGroup, caEndGroup, since these don't
;                        work with the non-callback version of caput, which
;                        we are now using (ezcaPutOldCa)
;-
    self->mca::set_presets, presets
    t = caput( self.record_name + '.PRTM',  self.presets.real_time)
    t = caput( self.record_name + '.PLTM',  self.presets.live_time)
    t = caput( self.record_name + '.PCT',   self.presets.total_counts)
    t = caput( self.record_name + '.PCTL',  self.presets.start_channel)
    t = caput( self.record_name + '.PCTH',  self.presets.end_channel)
    t = caput( self.record_name + '.DWEL',  self.presets.dwell)
    t = caput( self.record_name + '.CHAS',  self.presets.channel_advance)
    t = caput( self.record_name + '.PSCL',  self.presets.prescale)
end


;*****************************************************************************
function epics_mca::get_sequence
;+
; NAME:
;       EPICS_MCA::GET_SEQUENCE
;
; PURPOSE:
;       This function returns the current sequence number MCA.  Sequences
;       are used for time resolved spectroscopy, and refer to different regions
;       of MCA memory for data acquistion and readout.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;
;       Result = epics_mca->GET_SEQUENCE()
;
; PROCEDURE:
;       This function reads the preset information from the hardware using
;       the EPICS MCA record.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, November 18, 1997
;-
    temp = 0
    t = caget( self.record_name + '.SEQ',  temp)
    return, temp
end


;*****************************************************************************
pro epics_mca::set_sequence, sequence
;+
; NAME:
;      EPICS_MCA::SET_SEQUENCE
;
; PURPOSE:
;       This procedure sets the current sequence number for the MCA.  Sequences
;       are used for time resolved spectroscopy, and refer to different regions
;       of MCA memory for data acquistion and readout.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;
;       epics_mca->SET_SEQUENCE, Sequence
;
; PROCEDURE:
;       This function invokes writes the sequence number to the hardware
;       using the EPICS MCA record.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, November 18, 1997
;-
    t = caput( self.record_name + '.SEQ',  sequence)
end


;*****************************************************************************
function epics_mca::get_rois, roi_info, energy=energy
;+
; NAME:
;       EPICS_MCA::GET_ROIS
;
; PURPOSE:
;       This function returns the region-of-interest information for the MCA.
;       The rois information is contained in a structure of type MCA_ROIS.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;
;       Result = epics_mca->GET_ROIS(roi_info, /ENERGY)
;
; PROCEDURE:
;       This function reads the region-of-interest information from the
;       hardware using the EPICS MCA record, and then invokes MCA::GET_ROIS
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::GET_ROIS">MCA::GET_ROIS()</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;       19-Sep-1998 MLR  Added caStartGroup, caEndGroup for efficiency
;       25-Sep-1998 MLR  Added ENERGY keyword
;       28-Sep-2001 MLR  Re-wrote to use caGetArray, factor of 32 speed improvement
;-
    ; Read ROIs from record
    self.nrois = 0
    pvs = strarr(self.max_rois, 3)
    for i=0, self.max_rois-1 do begin
        ; Read the ROI numeric data
        pvs[i, 0] = self.record_name + '.R' + strtrim(string(i),2) + 'LO'
        pvs[i, 1] = self.record_name + '.R' + strtrim(string(i),2) + 'HI'
        pvs[i, 2] = self.record_name + '.R' + strtrim(string(i),2) + 'BG'
    endfor
;    t = caMonitor(pvs, lvals, /get, /long)
    t = caGetArray(pvs, lvals, /long)
    pvs = strarr(self.max_rois)
    for i=0, self.max_rois-1 do begin
        ; Read the string data
        pvs[i] = self.record_name + '.R' + strtrim(string(i),2) + 'NM'
    endfor
    t = caGetArray(pvs, svals, /string)
    ; Need to eliminate leading 1 dimension and make 3 rows
    lvals = reform(lvals, self.max_rois, 3)
    for i=0, self.max_rois-1 do begin
        left  = lvals[i, 0]
        right = lvals[i, 1]
        bgd_width = lvals[i, 2]
        label = string(svals[i])
        if ((left ge 0) and (right gt 0)) then begin
            roi = {MCA_ROI}
            roi.left = left
            roi.right = right
            roi.label = label
            roi.bgd_width = bgd_width
            roi.use = 1
            status = self->mca::add_roi(roi)
        endif
    endfor
    return, self->mca::get_rois(roi_info, energy=energy)
end


;*****************************************************************************
pro epics_mca::get_roi_counts, total, net
;+
; NAME:
;       EPICS_MCA::GET_ROI_COUNTS
;
; PURPOSE:
;       This procedures returns the net and total counts of each
;       region-of-interest in the MCA.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;
;       epics_mca->GET_ROI_COUNTS, Total, Net
;
; INPUTS:
;       None
;
; OUTPUTS:
;       Total:  The total counts in each ROI.
;       Net:    The net counts in each ROI.
;
;       NOTE: These values are computed by the record itself, not by this
;             routine or by the base class routine mca::get_roi_counts.
;             The dimension of each array is NROIS, where NROIS
;             is the number of currently defined ROIs for this MCA.  It returns
;             zero for both if NROIS is zero.  Users should call
;             MCA::GET_ROIS() to check the number of ROIS.
;
; EXAMPLE:
;       mca = obj_new('EPICS_MCA', '13IDC:mca1')
;       mca->GET_ROI_COUNTS, total, net
;       print, 'Net counts = ', net
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;-
    ; Read ROI counts from record
    nrois = self.nrois > 1
    total = fltarr(nrois)
    net = fltarr(nrois)
    pvs = strarr(self.nrois,2)
    for i=0, self.nrois-1 do begin
        pvs[i,0] =  self.record_name + '.R' + strtrim(string(i),2)
        pvs[i,1] =  self.record_name + '.R' + strtrim(string(i),2) + 'N'
    endfor
    t = caGetArray(pvs, vals)
    vals = reform(vals, self.nrois, 2)
    total = vals[*,0]
    net = vals[*,1]
end


;*****************************************************************************
function epics_mca::add_roi, roi, energy=energy
;+
; NAME:
;       EPICS_MCA::ADD_ROI
;
; PURPOSE:
;       This function adds a new region-of-interest to the MCA.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;
;       epics_mca->ADD_ROI, roi
;
; PROCEDURE:
;       This function invokes MCA::ADD_ROI and then calls EPICS_MCA::SET_ROIS
;       to write the new region-of-interest to the hardware the EPICS MCA
;       record.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::ADD_ROI">MCA::ADD_ROI</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;       23-Mar-1998     Mark Rivers.  Changed input parameter to a structure
;                       of type MCA_ROI from left, right, label, bgd_width
;-
    status = self->mca::add_roi(roi, energy=energy)
    if (self.nrois eq 0) then begin
        self->set_rois
    endif else begin
        self->set_rois, self.roi(0:self.nrois-1)
    endelse
    return, status
end


;*****************************************************************************
pro epics_mca::del_roi, index
;+
; NAME:
;       EPICS_MCA::DEL_ROI
;
; PURPOSE:
;       This function deletes a region-of-interest from the MCA.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;
;       epics_mca->DEL_ROI, Index
;
; PROCEDURE:
;       This function invokes MCA::DEL_ROI and then calls EPICS_MCA::SET_ROIS
;       to write the new region-of-interest to the hardware the EPICS MCA
;       record.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::DEL_ROI">MCA::DEL_ROI</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;       23-Mar-1998     Mark Rivers.  Modified input parameters to ROI index
;                       from left, right.
;
;-
    self->mca::del_roi, index
    if (self.nrois eq 0) then begin
        self->set_rois
    endif else begin
        self->set_rois, self.roi(0:self.nrois-1)
    endelse
end


;*****************************************************************************
pro epics_mca::set_rois, rois, energy=energy
;+
; NAME:
;      EPICS_MCA::SET_ROIS
;
; PURPOSE:
;       This procedure sets the region-of-interest information for the MCA.
;       The region-of-interest information is contained in an array of
;       structures of type MCA_ROIS.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;
;       epics_mca->SET_ROIS, Rois
;
; PROCEDURE:
;       This function invokes MCA::SET_ROIS and then writes the
;       ROI information to the hardware using the EPICS MCA record.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::SET_ROIS">MCA::SET_ROIS</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;       17-Sep-1998 MLR  Removed caStartGroup, caEndGroup, since these don't
;                        work with the non-callback version of caput, which
;                        we are now using (ezcaPutOldCa)
;       28-Sep-2001 MLR  Changed to using caPutArray, much faster.
;-
    self->mca::set_rois, rois, energy=energy
    pvs  = strarr(self.max_rois, 4)
    vals = strarr(self.max_rois, 4)
    for i=0, self.nrois-1 do begin
        pvs[i, 0]  = self.record_name + '.R' + strtrim(string(i),2) + 'LO'
        vals[i, 0] = self.roi(i).left
        pvs[i, 1]  = self.record_name + '.R' + strtrim(string(i),2) + 'HI'
        vals[i, 1] = self.roi(i).right
        pvs[i, 2]  = self.record_name + '.R' + strtrim(string(i),2) + 'NM'
        vals[i, 2] = self.roi(i).label
        pvs[i, 3]  = self.record_name + '.R' + strtrim(string(i),2) + 'BG'
        vals[i, 3] = self.roi(i).bgd_width
    endfor
    for i=self.nrois, self.max_rois-1 do begin
        pvs[i, 0]  = self.record_name + '.R' + strtrim(string(i),2) + 'LO'
        vals[i, 0] = "-1"
        pvs[i, 1]  = self.record_name + '.R' + strtrim(string(i),2) + 'HI'
        vals[i, 1] = "-1"
        pvs[i, 2]  = self.record_name + '.R' + strtrim(string(i),2) + 'NM'
        vals[i, 2] = " "
        pvs[i, 3]  = self.record_name + '.R' + strtrim(string(i),2) + 'BG'
        vals[i, 3] = 0
    endfor
    ; Need to make vals into a 1xN array, or caPutArray thinks we are writing
    ; an array of values to each PV
    vals = reform(vals, 1, self.max_rois*4)
    t = caPutArray(pvs, vals)
end


;*****************************************************************************
function epics_mca::get_environment, count, name=name, description=description
;+
; NAME:
;       EPICS_MCA::GET_ENVIRONMENT
;
; PURPOSE:
;       This function gets the environment parameters for the MCA.
;       The environment information is contained in an array of structures 
;       of type MCA_ENVIRONMENT.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       Result = epics_mca->GET_ENVIRONMENT()
;
; PROCEDURE:
;       This function reads the environment information from EPICS 
;       and then invokes MCA::GET_ENVIRONMENT
;
; KEYWORD PARAMETERS:
;       This function accepts all of the keyword paramters used by 
;       MCA::GET_ENVIRONMENT
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::GET_ENVIRONMENT">MCA::GET_ENVIRONMENT()</A>.
;
; EXAMPLE:
;       mca = obj_new('EPICS_MCA', '13IDC:aim_adc1')
;       env = mca->GET_ENVIRONMENT(count)
;       ; Get all environment variables
;       help, /structure, env[0]
;       ; Get all of the sample information
;       env = mca->GET_ENVIRONMENT(DESCRIPTION='Sample', count)
;       for i=0, count-1 do print, env[i].name, '=', env[i].value
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 29, 2001.  Put this code here
;                       and deleted from the MCA::WRITE_FILE routines.
;-
    if (ptr_valid(self.environment)) then begin
        n_env = n_elements(*self.environment)
        status = caGetArray((*self.environment).name, values, /string)
        (*self.environment).value = reform(values)
    endif
    return, self->mca::get_environment(count, $
                                       name=name, description=description)
end

;*****************************************************************************
function epics_mca::get_data, new_flag, check_new=check_new
;+
; NAME:
;       EPICS_MCA::GET_DATA
;
; PURPOSE:
;       This function returns the data from the MCA.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;       Result = epics_mca->GET_DATA()
;
; KEYWORD_PARAMETERS:
;       CHECK_NEW:
;           A flag which indicates that this routine should only return
;           the data if it has changed.
;
; OPTIONAL OUTPUTS:
;       NEW_FLAG:
;           If CHECK_FLAG is set, then NEW_FLAG will be 1 if the function
;           is returning new data, 0 if the function is not returning new
;           data.  If CHECK_FLAG is set and NEW_FLAG is 0 then the function
;           returns -1.
; PROCEDURE:
;       This function reads the data from the hardware using the EPICS MCA
;       record, and then invokes MCA::GET_DATA
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::GET_DATA">MCA::GET_DATA()</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;       Nov. 14, 1997   Mark Rivers.  Changed routine to eliminate setting
;                       rec.READ back to 0, since record support does this
;                       automatically and it was causing record to process
;                       again.
;       19-Sep-1998 MLR  Added /WAIT to caput, since default is not to wait
;                        for callback now.
;       17-Mar-1999 MLR  Removed /WAIT from caput, to be compatible with
;                        version 4.3 and later of the MCA record, which does
;                        not fire forward links until acquisition is complete.
;       28-Mar-1999 MLR  Changed routine so it no longer pokes READ field.
;                        This assumes that someone else (typically a database)
;                        is periodically poking the READ field.  The object
;                        initialization code now sets a monitor on the VAL
;                        field. Added New_flag output and CHECK_NEW keyword.
;-
    if keyword_set(CHECK_NEW) then begin
        new_flag = cacheckmonitor(self.record_name + '.VAL')
        if (new_flag eq 0) then return, -1
    endif
    self.nchans = 2048
    t = caget( self.record_name + '.NUSE', temp)
    if (t eq 0 ) then self.nchans = temp

    t = caget( self.record_name + '.VAL', temp, max=self.nchans)
    if (t eq 0) then self.data = temp
    return, self.data(0:self.nchans-1)
end


;*****************************************************************************
pro epics_mca::set_data, data
;+
; NAME:
;       EPICS_MCA::SET_DATA
;
; PURPOSE:
;       This procedure writes data to the MCA
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;       epics_mca->SET_DATA, Data
;
; PROCEDURE:
;       This function calls MCA::SET_DATA and then writes the data to the
;       EPICS MCA record.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::SET_DATA">MCA::SET_DATA()</A>.
;
; RESTRICTIONS:
;       This procedure writes the spectral data to the EPICS MCA record.
;       However the MCA record does not yet write the values to the MCA hardware
;       for any supported devices.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, April 29, 2000
;-
    self->mca::set_data, data
    n_chans = n_elements(data)
    t = caget( self.record_name + '.NMAX', max_chans)
    if (t eq 0 ) then begin
        n_chans = n_chans < max_chans
        t = caput( self.record_name + '.NUSE', n_chans, /WAIT)
        t = caput( self.record_name + '.VAL', data[0:n_chans-1], /WAIT)
    endif
end


;*****************************************************************************
function epics_mca::get_acquire_status, update=update, $
                    new_flag, check_new=check_new
;+
; NAME:
;       EPICS_MCA::GET_ACQUIRE_STATUS
;
; PURPOSE:
;       This function returns the acquisition status for the MCA.
;       This is 1 if the MCA is acquiring and 0 if it is not acquiring.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;       Result = epics_mca->GET_ACQUIRE_STATUS()
;
; KEYWORD PARAMETERS:
;   UPDATE:
;       Set this keyword to force the routine to process the record.
;       By default this routine does NOT process the record.  The
;       acquire status returned will be the status when the record
;       was last processed.
;   CHECK_NEW:
;       A flag which indicates that this routine should only return
;       the acquire status if it has changed.
;
; OPTIONAL OUTPUTS:
;   NEW_FLAG:
;       If CHECK_FLAG is set, then NEW_FLAG will be 1 if the function
;       is returning new acquire status, 0 if the function is not
;       returning new acquire status. If CHECK_FLAG is set and
;       NEW_FLAG is 0 then the function returns -1.
;
; PROCEDURE:
;       This function reads the acquisition status from the hardware using
;       the EPICS MCA record, and then invokes MCA::GET_ACQUIRE_STATUS
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::GET_ACQUIRE_STATUS">MCA::GET_ACQUIRE_STATUS()</A>.
;
; RESTRICTIONS:
;       This routine does NOT force the record to process by default.
;       The status will not update unless the record is processed.  This can
;       be done by setting the UPDATE keyword.
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;       Nov. 14, 1997   Mark Rivers added UPDATE keyword, changed routine to not
;                       process record by default.
;       19-Sep-1998 MLR  Added /WAIT since that is now longer default
;       17-Mar-1999 MLR  Removed /WAIT from caput, to be compatible with
;                        version 4.3 and later of the MCA record, which does
;                        not fire forward links until acquisition is complete.
;       15-Apr-1999 MLR Added CHECK_NEW and NEW_FLAG
;-
    if (keyword_set(update)) then $
            t = caput(self.record_name + '.PROC', 1)

    if keyword_set(CHECK_NEW) then begin
        new_flag = cacheckmonitor(self.record_name + '.ACQG')
        if (new_flag eq 0) then return, -1
    endif
    t = caget(self.record_name + '.ACQG', busy)
    if (t eq 0) then self.acquiring = busy
    return, self->mca::get_acquire_status()

end


;*****************************************************************************
pro epics_mca::acquire_wait, dwell_time, start=start, stop=stop
;+
; NAME:
;       EPICS_MCA::ACQUIRE_WAIT
;
; PURPOSE:
;       This procedures waits for acquisition of the MCA to complete.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;
;       epics_mca->ACQUIRE_WAIT, Time
;
; OPTIONAL INPUTS:
;       Time:  The estimated acquisition time of the MCA.
;
; KEYWORD_PARAMETERS:
;       START:
;           Set this flag to wait for acquisition to start.
;       STOP:
;           Set this flag to wait for acquisition to stop.  This is the default.
;
;       If both the START and STOP keywords are given then the routine will wait
;       first for acquisition to start and then for acquistion to stop.  If only
;       /START is given then it will not wait for acquisition to stop.
;       
;
; OUTPUTS:
;       None
;
; PROCEDURE:
;       This routine simply polls to see if acquisition is complete using
;       EPICS_MCA::GET_ACQUIRE_STATUS(). If the optional Time input is
;       specified then the time between polling is Time/10.  The default and
;       minimum time between polling is one second.
;
; EXAMPLE:
;       mca = obj_new('EPICS_MCA', '13IDC:mca1')
;       mca->ACQUIRE_ON
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;       MLR 18-APR-2001 Added START and STOP keywords
;-
    if (n_elements(dwell_time) eq 0) then dwell_time = 1.
    if ((n_elements(start) eq 0) and (n_elements(stop) eq 0)) then stop=1

    if (keyword_set(start)) then begin
        while (1) do begin
            if (self->get_acquire_status(/update) eq 1) then goto, done_start
            wait, dwell_time/100. < 1.0  ; Wait for dwell time or 1 second,
                                        ; whichever is less
        endwhile
    endif

    done_start:
    if (keyword_set(stop)) then begin
        while (1) do begin
            if (self->get_acquire_status(/update) eq 0) then return
            wait, dwell_time/100. < 1.0  ; Wait for dwell time or 1 second,
                                        ; whichever is less
        endwhile
    endif
end


;*****************************************************************************
pro epics_mca::erase
;+
; NAME:
;       EPICS_MCA::ERASE
;
; PURPOSE:
;       This procedures erases the MCA data, i.e. sets all channels to zero.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;
;       epics_mca->ERASE
;
; INPUTS:
;       None
;
; OUTPUTS:
;       None
;
; PROCEDURE:
;       This procedure erases the MCA by sending the appropriate command
;       to the EPICS MCA record.
;
; EXAMPLE:
;       mca = obj_new('EPICS_MCA', '13IDC:mca1')
;       mca->ERASE
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;       May 5, 1998:    Mark Rivers, removed setting the acquisition start time
;                       here, since the record now does that.
;       19-Sep-1998 MLR  Added /WAIT since that is now longer default
;       17-Mar-1999 MLR  Removed /WAIT from caput, to be compatible with
;                        version 4.3 and later of the MCA record, which does
;                        not fire forward links until acquisition is complete.
;-
    t = caput(self.record_name + '.ERAS', 1)
end


;*****************************************************************************
pro epics_mca::acquire_on
;+
; NAME:
;       EPICS_MCA::ACQUIRE_ON
;
; PURPOSE:
;       This procedures turns on acquisition of the MCA.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;
;       epics_mca->ACQUIRE_ON
;
; INPUTS:
;       None
;
; OUTPUTS:
;       None
;
; EXAMPLE:
;       mca = obj_new('EPICS_MCA', '13IDC:mca1')
;       mca->ACQUIRE_ON
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;       May 5, 1998:    Mark Rivers, removed setting the acquisition start time
;                       here, since the record now does that.
;       19-Sep-1998 MLR  Added /WAIT since that is now longer default
;       17-Mar-1999 MLR  Removed /WAIT from caput, to be compatible with
;                        version 4.3 and later of the MCA record, which does
;                        not fire forward links until acquisition is complete.
;-
    t = caput(self.record_name + '.STRT', 1)
end


;*****************************************************************************
pro epics_mca::acquire_off
;+
; NAME:
;       EPICS_MCA::ACQUIRE_OFF
;
; PURPOSE:
;       This procedures turns off acquisition of the MCA.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;
;       epics_mca->ACQUIRE_OFF
;
; INPUTS:
;       None
;
; OUTPUTS:
;       None
;
; EXAMPLE:
;       mca = obj_new('EPICS_MCA', '13IDC:mca1')
;       mca->ACQUIRE_OFF
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;       19-Sep-1998 MLR  Added /WAIT since that is now longer default
;       17-Mar-1999 MLR  Removed /WAIT from caput, to be compatible with
;                        version 4.3 and later of the MCA record, which does
;                        not fire forward links until acquisition is complete.
;-
    t = caput(self.record_name + '.STOP', 1)
end


;*****************************************************************************
pro epics_mca::write_file, file
;+
; NAME:
;       EPICS_MCA::WRITE_FILE
;
; PURPOSE:
;       This procedure writes an EPIC_MCA object to a disk file.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;
;       epics_mca->WRITE_FILE, file
;
; PROCEDURE:
;       This function reads all information from the hardware using the EPICS
;       MCA record, and then invokes MCA::WRITE_FILE
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::WRITE_FILE">MCA::WRITE_FILE</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, January 17, 1998
;       MLR 29-SEP-2001 Removed calls to read elapsed, calibration, rois, and
;                       data, since this is done in MCA::WRITE_FILE.
;-
    self->mca::write_file, file
    ; Reset the client wait flag in case it is set
    t = caput(self.record_name + 'ClientWait', 0)
end

;*****************************************************************************
pro epics_mca::spectra_scan, first_file, scan_record
;+
; NAME:
;       EPICS_MCA::SPECTRA_SCAN
;
; PURPOSE:
;       This procedures collects MCA spectra in conjunction with an EPICS
;       scan record.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;       epics_mca->SPECTRA_SCAN, First_file, Scan_record
;
; INPUTS:
;       First_file:  
;           The name of the first spectrum file to save.  Subsequent files 
;           will be named using the INCREMENT_FILENAME()function.  The 
;           filename must end in a numeric extension for this to work.
;
;       Scan_record:
;           The name of the EPIC scan record which is controlling the scan.
;           This scan record must start MCA data collection by writing "1" 
;           into the EraseStart record of the MCA database.
;       
; EXAMPLE:
;       mca = obj_new('EPICS_MCA', '13IDC:mca1')
;       mca->SPECTRA_SCAN, 'test.001', '13IDC:scan1'
;
; PROCEDURE:
;       1) Set scan.EnableWait to 1 so scan will wait for IDL client
;       2) Wait for scan.EXSC = 1, meaning scan has started
;       3) If scan.EXSC is still 0 (scan ended or aborted) disable EnableWait
;          and return
;       4) Read scan.ClientWait, if 0 (meaning acquisition has not started) go
;          to 3
;       5) Wait for MCA aquisition to start and then stop
;       6) Write data to disk with MCA::WRITE_FILE, increment file name
;       7) Reset ClientWait to 0 so scan will continue
;       8) Go to 3
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, April 18, 2001.  Copied from MED__DEFINE.
;-
    file = first_file

;   Enable waiting for client
    t = caput(self.record_name + 'EnableWait', 1)

;   Wait for scan to start
    repeat t = caget(scan_record + '.EXSC', started) until (started)

    while (1) do begin
        repeat begin
            ; If scan is complete, return
            t = caget(scan_record + '.EXSC', scan_status)
            if (scan_status eq 0) then begin
                ; Disable waiting for client
                t = caput(self.record_name + 'EnableWait', 0)
                return
            endif
            ; Wait for scan record to start acquisition
            t = caget(self.record_name + 'ClientWait', started) 
        endrep until (started)

        ; Wait for MCA to report start of acquisition, then wait for done
        self->acquire_wait, /start, /stop

        ; Write file
        self->write_file, file
        print, 'Saved file: ', file
        file = increment_filename(file)

        ; Reset the client wait flag
        t = caput(self.record_name + 'ClientWait', 0)  
    endwhile

end


;*****************************************************************************
function epics_mca::init, record_name, environment_file=environment_file
;+
; NAME:
;       EPICS_MCA::INIT
;
; PURPOSE:
;       This is the initialization code which is invoked when a new object of
;       type EPICS_MCA is created.  It cannot be called directly, but only
;       indirectly by the IDL OBJ_NEW() function.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;       Result = OBJ_NEW('EPICS_MCA', Record_Name)
;
; INPUTS:
;       Record_Name:  The name of the EPICS MCA record for the MCA object
;                     being created.  This record name can include a field
;                     name which will be stripped off.  For example,
;                     '13IDC:mca1' and '13IDC:mca1.DESC' are both
;                     valid.  This makes it convenient when dragging process
;                     variable names from MEDM windows to IDL windows.
;
; KEYWORD_PARAMETERS:
;       Environment_file:
;           This keyword can be used to specify the name of a file which
;           contains the names of EPICS process variables which should be saved
;           in the header of files written with MCA::WRITE_FILE.  If this
;           keyword is not specified then this function will attempt to open
;           a file called 'catch1d.env' in the current directory.  This is done
;           to be compatible with the data catcher program.  This is an ASCII
;           with each line containing a process variable name, followed by a
;           space and a description field.
;
; OUTPUTS:
;       This function returns a status to indicate whether it was able to
;       establish channel access communication with the specified EPICS MCA
;       record.  This status is 1 for success, 0 for failure.  This status is
;       passed back indirectly to the routine which calls OBJ_NEW().  OBJ_NEW
;       will return a valid object pointer if this routine succeeds, and will
;       return a NULL object pointer if this routine fails.  The user should
;       test the return value of OBJ_NEW() with the IDL function OBJ_VALID().
;
; SIDE EFFECTS:
;       The routine establishes channel access monitors on all of the fields
;       in the MCA record which the methods in this class will read.  This
;       greatly improves the speed and efficiency.
;
; RESTRICTIONS:
;       This routine cannot be called directly.  It is called indirectly when
;       creating a new object of class EPICS_MCA by the IDL OBJ_NEW()
;       function.
;
; EXAMPLE:
;       mca = obj_new('EPICS_MCA', '13IDC:mca1')
;       if (OBJ_VALID(mca)) then print, 'It worked!'
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;       MLR 18-Nov-1997  Removed CA monitor on .VAL field, because old data
;                        were being read when doing things quickly.
;       MLR 22-Oct-1998  Added environment_file
;       MLR 28-Mar-1999  Put monitor on VAL field so get_data() can return
;                        a flag indicating if there is new data.
;       MLR 15-Apr-1999  Put monitor on STIM field since we now use this to get
;                        acquisition start time.
;-
    t = self->mca::init()
    ; Make sure ezca has been initialized
    cainit
    self.record_name = record_name
    self.name = record_name
    status = caget( self.record_name + '.NUSE', nuse) ; see if it exists
    if status ne 0 then begin                       ; it does not exist
        return, 0
    endif
    self.nchans = nuse > 1
    self.data(0 : self.nchans-1L) = 0L
    ; Set channel access monitors on all fields we will be reading
    ; with simple caGet, or with caCheckMonitor
    t = casetmonitor(self.record_name + '.ACQG')
    t = casetmonitor(self.record_name + '.VAL')
    t = casetmonitor(self.record_name + '.NUSE')
    t = casetmonitor(self.record_name + '.ERTM')

;   Need to read calibration initially so that MCA::CHAN_TO_ENERGY, etc. work
    t = self->get_calibration()
    t = self->get_presets()  ; May not need to read presets, does not hurt
    t = self->get_rois()     ; Need to do so MCA::ADD_ROI, etc. will work

;   Read the environment file
    if n_elements(environment_file) eq 0 then environment_file = 'catch1d.env'
    ;print, 'Epics MCA Opening envfile ', environment_file
    openr, lun, /get, environment_file, error=error
    if (error eq 0) then begin
        temp = {mca_environment}
        while (not eof(lun)) do begin
            line = ''
            readf, lun, line
            line = strtrim(line,2)
            if (strlen(line) ge 1) then begin
                pos = strpos(line, ' ')
                if (pos ne -1) then begin
                    temp.name = strmid(line, 0, pos)
                    temp.description = strtrim(strmid(line, pos, 1000), 2)
                endif else begin
                    temp.name = line
                    temp.description = ' '
                endelse
                if (n_elements(env) eq 0) then env = temp else env = [env, temp]
            endif
        endwhile
        self.environment = ptr_new(env, /no_copy)
        close, lun
        free_lun, lun
    endif
    return, 1
end


;*****************************************************************************
pro epics_mca__define
;+
; NAME:
;       EPICS_MCA__DEFINE
;
; PURPOSE:
;       This is the definition code which is invoked when a new object of
;       type EPICS_MCA is created.  It cannot be called directly, but only
;       indirectly by the IDL OBJ_NEW() function,
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;       Result = OBJ_NEW('EPICS_MCA', Record_Name)
;
; INPUTS:
;       Record_Name:  The name of the EPICS MCA record for the MCA object
;                     being created.  This record name can include a field
;                     name which will be stripped off.  For example,
;                     '13IDC:mca1' and '13IDC:mca1.DESC' are both
;                     valid.  This makes it convenient when dragging process
;                     variable names from MEDM windows to IDL windows.  This
;                     name is passed to EPICS_MCA::INIT().
;
; OUTPUTS:
;       None (but see EPICS_MCA::INIT)
;
; RESTRICTIONS:
;       This routine cannot be called directly.  It is called indirectly when
;       creating a new object of class EPICS_MCA by the IDL OBJ_NEW()
;       function.
;
; EXAMPLE:
;       mca = obj_new('EPICS_MCA', '13IDC:mca1')
;       if (OBJ_VALID(mca)) then print, 'It worked!'
;
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;-
    epics_mca = { epics_mca, $
            record_name: '', $
            INHERITS mca}
end
