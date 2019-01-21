;*****************************************************************************
function genie_mca::get_calibration
;+
; NAME:
;       GENIE_MCA::GET_CALIBRATION
;
; PURPOSE:
;       This function returns the calibration parameters for the MCA.
;       The calibration information is contained in a structure of type
;       MCA_CALIBRATION.
;
; CATEGORY:
;       GENIE device class library.
;
; CALLING SEQUENCE:
;
;       Result = genie_mca->GET_CALIBRATION()
;
; PROCEDURE:
;       This function reads the calibration information from the hardware using
;       the GENIE MCA detector, and then invokes MCA::GET_CALIBRATION
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::GET_CALIBRATION">MCA::GET_CALIBRATION()</A>.
;
; MODIFICATION HISTORY:
;       Written by:         Mark Rivers, February 23, 1999
;       March 9, 2001 MLR   Incorporated changes (previously made at NSLS)
;                           to make DLL location an environment variable.
;                            
;-
    offset = 0.
    slope = 0.
    quad = 0.
    units = bytarr(100)
    two_theta = 0.
    t = call_external(self.dll_name, 'get_calibration', self.hDSC, $
                      offset, slope, quad, units, two_theta)
    self.calibration.offset = offset
    self.calibration.slope = slope
    self.calibration.quad = quad
    self.calibration.units = string(units)
    self.calibration.two_theta = two_theta
    return, self->mca::get_calibration()
end


;*****************************************************************************
pro genie_mca::set_calibration, calibration
;+
; NAME:
;       GENIE_MCA::SET_CALIBRATION
;
; PURPOSE:
;       This procedure sets the calibration parameters for the MCA.
;       The calibration information is contained in a structure of type
;       MCA_CALIBRATION.
;
; CATEGORY:
;       GENIE device class library.
;
; CALLING SEQUENCE:
;
;       genie_mca->SET_CALIBRATION, Calibration
;
; PROCEDURE:
;       This function invokes MCA::SET_CALIBRATION and then writes the
;       calibration information to the hardware using the GENIE MCA detector.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::SET_CALIBRATION">MCA::SET_CALIBRATION</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, February 23, 1999
;-
    self->mca::set_calibration, calibration
    units = [ byte(self.calibration.units), 0B]
    t = call_external(self.dll_name, 'set_calibration', self.hDSC, $
                      self.calibration.offset, $
                      self.calibration.slope, $
                      self.calibration.quad, $
                      units, $
                      self.calibration.two_theta)
end


;*****************************************************************************
function genie_mca::get_elapsed, new_flag, check_new=check_new
;+
; NAME:
;       GENIE_MCA::GET_ELAPSED
;
; PURPOSE:
;       This function returns the elapsed parameters for the MCA.
;       The elapsed information is contained in a structure of type
;       MCA_ELAPSED.
;
; CATEGORY:
;       GENIE device class library.
;
; CALLING SEQUENCE:
;       Result = genie_mca->GET_ELAPSED()
;
; KEYWORD_PARAMETERS:
;       CHECK_NEW:
;           A flag which indicates that this routine should only return
;           the elapsed parameters if they have changed.
;           NOTE: This is not yet implemented.
;
; OPTIONAL OUTPUTS:
;       NEW_FLAG:
;           If CHECK_FLAG is set, then NEW_FLAG will be 1 if the function
;           is returning new elapsed parameters, 0 if the function is not
;           returning new elapsed parameters.  If CHECK_FLAG is set and
;           NEW_FLAG is 0 then the function returns -1.
;           NOTE: The CHECK_NEW logic is not yet implemented, so NEW_FLAG
;           will always be set to 1 if CHECK_NEW is set.
;
; PROCEDURE:
;       This function reads the elapsed information from the hardware using
;       the GENIE MCA detector, and then invokes MCA::GET_ELAPSED
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::GET_ELAPSED">MCA::GET_ELAPSED()</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, February 23, 1999
;       08-Feb-2000  Mark Rivers.  Added .READ_TIME, CHECK_NEW
;-
    if (keyword_set(CHECK_NEW)) then new_flag = 1
    real = 0.D0
    live = 0.D0
    tot = 0L
    start_time = bytarr(100)
    t = call_external(self.dll_name, 'get_elapsed', self.hDSC, $
                      real, live, tot, start_time)
    self.elapsed.real_time = real
    self.elapsed.live_time = live
    self.elapsed.total_counts = tot
    self.elapsed.start_time = string(start_time)
    self.elapsed.read_time = systime(1)
    return, self->mca::get_elapsed()
end


;*****************************************************************************
function genie_mca::get_presets
;+
; NAME:
;       GENIE_MCA::GET_PRESETS
;
; PURPOSE:
;       This function returns the preset parameters for the MCA.
;       The preset information is contained in a structure of type
;       MCA_PRESETS.
;
; CATEGORY:
;       GENIE device class library.
;
; CALLING SEQUENCE:
;
;       Result = genie_mca->GET_PRESETS()
;
; PROCEDURE:
;       This function reads the preset information from the hardware using
;       the GENIE MCA detector, and then invokes MCA::GET_PRESETS
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::GET_PRESETS">MCA::GET_PRESETS()</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, February 23, 1999
;-
    real = 0.D0
    live = 0.D0
    counts = 0L
    low = 0L
    high = 0L
    t = call_external(self.dll_name, 'get_presets', self.hDSC, $
                      real, live, counts, low, high)
    self.presets.real_time = real
    self.presets.live_time = live
    self.presets.total_counts = counts
    self.presets.start_channel = low
    self.presets.end_channel = high
    return, self->mca::get_presets()
end


;*****************************************************************************
pro genie_mca::set_presets, presets
;+
; NAME:
;      GENIE_MCA::SET_PRESETS
;
; PURPOSE:
;       This procedure sets the preset parameters for the MCA.
;       The preset information is contained in a structure of type
;       MCA_PRESETS.
;
; CATEGORY:
;       GENIE device class library.
;
; CALLING SEQUENCE:
;
;       genie_mca->SET_PRESETS, Presets
;
; PROCEDURE:
;       This function invokes MCA::SET_PRESETS and then writes the
;       preset information to the hardware using the GENIE MCA detector.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::SET_PRESETS">MCA::SET_PRESETS</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, February 23, 1999
;-
    self->mca::set_presets, presets
    t = call_external(self.dll_name, 'set_presets', self.hDSC, $
                      double(self.presets.real_time), $
                      double(self.presets.live_time), $
                      self.presets.total_counts, $
                      self.presets.start_channel, $
                      self.presets.end_channel)
end


;*****************************************************************************
function genie_mca::get_sequence
;+
; NAME:
;       GENIE_MCA::GET_SEQUENCE
;
; PURPOSE:
;       This function returns the current sequence number MCA.  Sequences
;       are used for time resolved spectroscopy, and refer to different regions
;       of MCA memory for data acquistion and readout.
;
; CATEGORY:
;       GENIE device class library.
;
; CALLING SEQUENCE:
;
;       Result = genie_mca->GET_SEQUENCE()
;
; PROCEDURE:
;       This function reads the sequence information from the hardware using
;       the GENIE MCA detector.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, February 23, 1999
;-
    sequence = 0L
    t = call_external(self.dll_name, 'get_sequence', self.hDSC, sequence)
    return, sequence
end


;*****************************************************************************
pro genie_mca::set_sequence, sequence
;+
; NAME:
;      GENIE_MCA::SET_SEQUENCE
;
; PURPOSE:
;       This procedure sets the current sequence number for the MCA.  Sequences
;       are used for time resolved spectroscopy, and refer to different regions
;       of MCA memory for data acquistion and readout.
;
; CATEGORY:
;       GENIE device class library.
;
; CALLING SEQUENCE:
;
;       genie_mca->SET_SEQUENCE, Sequence
;
; PROCEDURE:
;       This function invokes writes the sequence number to the hardware
;       using the GENIE MCA detector.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, February 23, 1999
;-
    t = call_external(self.dll_name, 'set_sequence', self.hDSC, $
                      long(seqence))
end


;*****************************************************************************
function genie_mca::get_rois, roi_info, energy=energy
;+
; NAME:
;       GENIE_MCA::GET_ROIS
;
; PURPOSE:
;       This function returns the region-of-interest information for the MCA.
;       The rois information is contained in a structure of type MCA_ROIS.
;
; CATEGORY:
;       GENIE device class library.
;
; CALLING SEQUENCE:
;
;       Result = genie_mca->GET_ROIS(roi_info, /ENERGY)
;
; PROCEDURE:
;       This function reads the region-of-interest information from the
;       hardware using the GENIE MCA detector, and then invokes MCA::GET_ROIS
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::GET_ROIS">MCA::GET_ROIS()</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, February 28, 1999
;-
;    ; Read ROIs from detector
    self.nrois = 0
    nrois = long(self.max_rois)
 ; Passed as max on input, actual on output
    left = lonarr(self.max_rois)
    right = lonarr(self.max_rois)
    label = bytarr(17, self.max_rois)
    width = lonarr(self.max_rois)
    t = call_external(self.dll_name, 'get_rois', self.hDSC, nrois, $
                      left, right, label, width)
    for i=0, nrois-1 do begin
        if ((left[i] gt 0) and (right[i] gt 0)) then begin
            roi = {MCA_ROI}
            roi.left = left[i]-1
            roi.right = right[i]-1
            roi.label = string(label[*,i])
            roi.bgd_width = width[i]
            roi.use = 1
            status = self->mca::add_roi(roi)
        endif
    endfor
    return, self->mca::get_rois(roi_info, energy=energy)
end


;*****************************************************************************
pro genie_mca::get_roi_counts, total, net
;+
; NAME:
;       GENIE_MCA::GET_ROI_COUNTS
;
; PURPOSE:
;       This procedures returns the net and total counts of each
;       region-of-interest in the MCA.
;
; CATEGORY:
;       GENIE device class library.
;
; CALLING SEQUENCE:
;
;       genie_mca->GET_ROI_COUNTS, Total, Net
;
; INPUTS:
;       None
;
; OUTPUTS:
;       Total:  The total counts in each ROI.
;       Net:    The net counts in each ROI.
;
;       NOTE: The dimension of each array is NROIS, where NROIS is the
;             number of currently defined ROIs for this MCA.  It returns
;             zero for both if NROIS is zero.  Users should call
;             MCA::GET_ROIS() to check the number of ROIS.
;
; EXAMPLE:
;       mca = obj_new('GENIE_MCA', 'DET01')
;       mca->GET_ROI_COUNTS, total, net
;       print, 'Net counts = ', net
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;-
;       WORKED NEEDED!!!!
;       MUST COMPUTE ROI COUNTS HERE - IT WAS DONE BY THE RECORD IN EPICS_MCA
;
end


;*****************************************************************************
function genie_mca::add_roi, roi, energy=energy
;+
; NAME:
;       GENIE_MCA::ADD_ROI
;
; PURPOSE:
;       This function adds a new region-of-interest to the MCA.
;
; CATEGORY:
;       GENIE device class library.
;
; CALLING SEQUENCE:
;
;       genie_mca->ADD_ROI, roi
;
; PROCEDURE:
;       This function invokes MCA::ADD_ROI and then calls GENIE_MCA::SET_ROIS
;       to write the new region-of-interest to the hardware the GENIE MCA
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
pro genie_mca::del_roi, index
;+
; NAME:
;       GENIE_MCA::DEL_ROI
;
; PURPOSE:
;       This function deletes a region-of-interest from the MCA.
;
; CATEGORY:
;       GENIE device class library.
;
; CALLING SEQUENCE:
;
;       genie_mca->DEL_ROI, Index
;
; PROCEDURE:
;       This function invokes MCA::DEL_ROI and then calls GENIE_MCA::SET_ROIS
;       to write the new region-of-interest to the hardware the GENIE MCA
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
pro genie_mca::set_rois, rois, energy=energy
;+
; NAME:
;      GENIE_MCA::SET_ROIS
;
; PURPOSE:
;       This procedure sets the region-of-interest information for the MCA.
;       The region-of-interest information is contained in an array of
;       structures of type MCA_ROIS.
;
; CATEGORY:
;       GENIE device class library.
;
; CALLING SEQUENCE:
;
;       genie_mca->SET_ROIS, Rois
;
; PROCEDURE:
;       This function invokes MCA::SET_ROIS and then writes the
;       ROI information to the hardware using the GENIE MCA record.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::SET_ROIS">MCA::SET_ROIS</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Feb. 28, 1999
;-
    self->mca::set_rois, rois, energy=energy
    label = bytarr(17, self.max_rois)
    for i=0, self.nrois-1 do begin
        label[0,i] = [byte(self.roi[i].label), 0B]
    endfor
    t = call_external(self.dll_name, 'set_rois', self.hDSC, long(self.nrois), $
                      long(self.roi.left + 1), long(self.roi.right + 1), $
                      label, long(self.roi.bgd_width))
end


;*****************************************************************************
function genie_mca::get_data, new_flag, check_new=check_new
;+
; NAME:
;       GENIE_MCA::GET_DATA
;
; PURPOSE:
;       This function returns the data from the MCA.
;
; CATEGORY:
;       GENIE device class library.
;
; CALLING SEQUENCE:
;       Result = genie_mca->GET_DATA()
;
; KEYWORD_PARAMETERS:
;       CHECK_NEW:
;           A flag which indicates that this routine should only return
;           the data if it has changed.
;           NOTE: This is not yet implemented.
;
; OPTIONAL OUTPUTS:
;       NEW_FLAG:
;           If CHECK_FLAG is set, then NEW_FLAG will be 1 if the function
;           is returning new data, 0 if the function is not returning new
;           data.  If CHECK_FLAG is set and NEW_FLAG is 0 then the function
;           returns -1.
;           NOTE: The CHECK_NEW logic is not yet implemented, so NEW_FLAG
;           will always be set to 1 if CHECK_NEW is set.
;
; PROCEDURE:
;       This function reads the data from the hardware using the GENIE MCA
;       detector, and then invokes MCA::GET_DATA
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::GET_DATA">MCA::GET_DATA()</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, February 23, 1999
;-
    if (keyword_set(CHECK_NEW)) then new_flag = 1
    data = lonarr(self.nchans)
    start_chan = 1
    t = call_external(self.dll_name, 'read_data', self.hDSC, $
                      start_chan, fix(self.nchans), data)
    self.data = data
    return, self->mca::get_data()
end


;*****************************************************************************
function genie_mca::get_acquire_status, update=update, $
                    new_flag, check_new=check_new
;+
; NAME:
;       GENIE_MCA::GET_ACQUIRE_STATUS
;
; PURPOSE:
;       This function returns the acquisition status for the MCA.
;       This is 1 if the MCA is acquiring and 0 if it is not acquiring.
;
; CATEGORY:
;       GENIE device class library.
;
; CALLING SEQUENCE:
;
;       Result = genie_mca->GET_ACQUIRE_STATUS()
;
; KEYWORD PARAMETERS:
;   UPDATE: This keyword exists only for compatibility with the EPICS_MCA
;           object.
;   CHECK_NEW:
;       A flag which indicates that this routine should only return
;       the acquire status if it has changed.
;       NOTE: This is not yet implemented.
;
; OPTIONAL OUTPUTS:
;   NEW_FLAG:
;       If CHECK_FLAG is set, then NEW_FLAG will be 1 if the function
;       is returning new acquire status, 0 if the function is not
;       returning new acquire status. If CHECK_FLAG is set and
;       NEW_FLAG is 0 then the function returns -1.
;       NOTE: The CHECK_NEW logic is not yet implemented, so NEW_FLAG
;       will always be set to 1 if CHECK_NEW is set.
;
; PROCEDURE:
;       This function reads the acquisition status from the hardware using
;       the GENIE MCA detector, and then invokes MCA::GET_ACQUIRE_STATUS
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::GET_ACQUIRE_STATUS">MCA::GET_ACQUIRE_STATUS()</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, February 23, 1999
;-
    if (keyword_set(CHECK_NEW)) then new_flag = 1
    busy = 0L
    t = call_external(self.dll_name, 'get_status', self.hDSC, busy)
    self.acquiring = busy
    return, self->mca::get_acquire_status()
end


;*****************************************************************************
pro genie_mca::acquire_wait, dwell_time
;+
; NAME:
;       GENIE_MCA::ACQUIRE_WAIT
;
; PURPOSE:
;       This procedures waits for acquisition of the MCA to complete.
;
; CATEGORY:
;       GENIE device class library.
;
; CALLING SEQUENCE:
;
;       genie_mca->ACQUIRE_WAIT, Time
;
; OPTIONAL INPUTS:
;       Time:  The estimated acquisition time of the MCA.
;
; OUTPUTS:
;       None
;
; PROCEDURE:
;       This routine simply polls to see if acquisition is complete using
;       GENIE_MCA::GET_ACQUIRE_STATUS(). If the optional Time input is
;       specified then the time between polling is Time/10.  The default and
;       minimum time between polling is one second.
;
; EXAMPLE:
;       mca = obj_new('GENIE_MCA', 'DET01')
;       mca->ACQUIRE_ON
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;-
    if (n_elements(dwell_time) eq 0) then dwell_time = 1.
    while (1) do begin
        if (self->get_acquire_status(/update) eq 0) then return
        wait, dwell_time/10. < 1.0  ; Wait for dwell time or 1 second,
                                    ; whichever is less
    endwhile
end


;*****************************************************************************
pro genie_mca::erase
;+
; NAME:
;       GENIE_MCA::ERASE
;
; PURPOSE:
;       This procedures erases the MCA data, i.e. sets all channels to zero.
;
; CATEGORY:
;       GENIE device class library.
;
; CALLING SEQUENCE:
;
;       genie_mca->ERASE
;
; INPUTS:
;       None
;
; OUTPUTS:
;       None
;
; PROCEDURE:
;       This procedure erases the MCA by sending the appropriate command
;       to the GENIE detector.
;
; EXAMPLE:
;       mca = obj_new('GENIE_MCA', 'DET01')
;       mca->ERASE
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, February 23, 1999
;-
    t = call_external(self.dll_name, 'erase', self.hDSC)
end


;*****************************************************************************
pro genie_mca::acquire_on
;+
; NAME:
;       GENIE_MCA::ACQUIRE_ON
;
; PURPOSE:
;       This procedures turns on acquisition of the MCA.
;
; CATEGORY:
;       GENIE device class library.
;
; CALLING SEQUENCE:
;
;       genie_mca->ACQUIRE_ON
;
; INPUTS:
;       None
;
; OUTPUTS:
;       None
;
; EXAMPLE:
;       mca = obj_new('GENIE_MCA', 'DET01')
;       mca->ACQUIRE_ON
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, February 23, 1999
;-
    t = call_external(self.dll_name, 'acquire_on', self.hDSC)
end


;*****************************************************************************
pro genie_mca::acquire_off
;+
; NAME:
;       GENIE_MCA::ACQUIRE_OFF
;
; PURPOSE:
;       This procedures turns off acquisition of the MCA.
;
; CATEGORY:
;       GENIE device class library.
;
; CALLING SEQUENCE:
;
;       genie_mca->ACQUIRE_OFF
;
; INPUTS:
;       None
;
; OUTPUTS:
;       None
;
; EXAMPLE:
;       mca = obj_new('GENIE_MCA', 'DET01')
;       mca->ACQUIRE_OFF
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, February 23, 1999
;-
    t = call_external(self.dll_name, 'acquire_off', self.hDSC)
end


;*****************************************************************************
pro genie_mca::write_file, file
;+
; NAME:
;       GENIE_MCA::WRITE_FILE
;
; PURPOSE:
;       This procedure writes an EPIC_MCA object to a disk file.
;
; CATEGORY:
;       GENIE device class library.
;
; CALLING SEQUENCE:
;
;       genie_mca->WRITE_FILE, file
;
; PROCEDURE:
;       This function reads all information from the hardware using the GENIE
;       MCA record, and then invokes MCA::WRITE_FILE
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::WRITE_FILE">MCA::WRITE_FILE</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, February 23, 1999
;-
    t = self->get_elapsed()
    t = self->get_rois()
    t = self->get_calibration()
    t = self->get_data()
    self->mca::write_file, file
end


;*****************************************************************************
pro genie_mca::cleanup
;+
; NAME:
;       GENIE_MCA::CLEANUP
;
; PURPOSE:
;       This is the cleanup code which is invoked when an object of
;       type GENIE_MCA is deleted.  It cannot be called directly, but only
;       indirectly by the IDL OBJ_DESTROY() function.
;
; CATEGORY:
;       GENIE device class library.
;
; CALLING SEQUENCE:
;       OBJ_DESTROY, Mca
;
; INPUTS:
;       Mca:
;           The name of the GENIE_MCA object being deleted.
;
; RESTRICTIONS:
;       This routine cannot be called directly.  It is called indirectly when
;       deleting object of class GENIE_MCA by the IDL OBJ_DESTROY()
;       function.
;
; EXAMPLE:
;       mca = obj_new('GENIE_MCA', 'DET01')
;       obj_destroy, mca
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, February 28, 1999
;-
    status = call_external(self.dll_name, 'cleanup', self.hDSC)
end

;*****************************************************************************
function genie_mca::init, detector_name, environment_file=environment_file
;+
; NAME:
;       GENIE_MCA::INIT
;
; PURPOSE:
;       This is the initialization code which is invoked when a new object of
;       type GENIE_MCA is created.  It cannot be called directly, but only
;       indirectly by the IDL OBJ_NEW() function.
;
; CATEGORY:
;       GENIE device class library.
;
; CALLING SEQUENCE:
;       Result = OBJ_NEW('GENIE_MCA', Detector_Name)
;
; INPUTS:
;       Detector_Name:
;           The name of the GENIE detector for the MCA object being created.
;
; KEYWORD_PARAMETERS:
;       Environment_file:
;           This keyword can be used to specify the name of a file which
;           contains the names of GENIE process variables which should be saved
;           in the header of files written with MCA::WRITE_FILE.  If this
;           keyword is not specified then this function will attempt to open
;           a file called 'catch1d.env' in the current directory.  This is done
;           to be compatible with the data catcher program.  This is an ASCII
;           with each line containing a process variable name, followed by a
;           space and a description field.
;
; OUTPUTS:
;       This function returns a status to indicate whether it was able to
;       open the specified GENIE MCA detector. This status is 1 for success,
;       0 for failure.  This status is passed back indirectly to the routine
;       which calls OBJ_NEW().  OBJ_NEW will return a valid object pointer if
;       this routine succeeds, and will return a NULL object pointer if this
;       routine fails.  The user should test the return value of OBJ_NEW()
;       with the IDL function OBJ_VALID().
;
; RESTRICTIONS:
;       This routine cannot be called directly.  It is called indirectly when
;       creating a new object of class GENIE_MCA by the IDL OBJ_NEW()
;       function.
;       NOTE: The environment function is not yet implemented.
;
; EXAMPLE:
;       mca = obj_new('GENIE_MCA', 'DET01')
;       if (OBJ_VALID(mca)) then print, 'It worked!'
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, February 23, 1999
;-
    t = self->mca::init()
    self.detector_name = detector_name
    self.name = detector_name
    self.dll_name = getenv('GENIE_IDL_DLL')
    hDSC = 0L
    nchans = 0L
    name = [byte(self.detector_name), 0B]
    status = call_external(self.dll_name, 'initialize', name, hDSC, nchans)
    if status ne 0 then return, 0
    self.hDSC = hDSC
    self.nchans = nchans
    self.data(0 : self.nchans-1L) = 0L
    ; Must read in calibration for MCA::CHAN_TO_ENERGY, etc. to work
    t = self->get_calibration()
;    t = self->get_presets()
;    t = self->get_rois()

;   Read the environment file
;    if n_elements(environment_file) eq 0 then environment_file = 'catch1d.env'
;    openr, lun, /get, environment_file, error=error
;    if (error eq 0) then begin
;        temp = {mca_environment}
;        while (not eof(lun)) do begin
;            line = ''
;            readf, lun, line
;            pos = strpos(line, ' ')
;            temp.name = strmid(line, 0, pos)
;            temp.description = strtrim(strmid(line, pos, 1000), 2)
;            if (n_elements(env) eq 0) then env = temp else env = [env, temp]
;        endwhile
;        self.environment = ptr_new(env, /no_copy)
;        free_lun, lun
;    endif
    return, 1
end


;*****************************************************************************
pro genie_mca__define
;+
; NAME:
;       GENIE_MCA__DEFINE
;
; PURPOSE:
;       This is the definition code which is invoked when a new object of
;       type GENIE_MCA is created.  It cannot be called directly, but only
;       indirectly by the IDL OBJ_NEW() function,
;
; CATEGORY:
;       GENIE device class library.
;
; CALLING SEQUENCE:
;       Result = OBJ_NEW('GENIE_MCA', Detector_name)
;
; INPUTS:
;       Detector_Name:
;           The name of the GENIE detector for the MCA object being created.
;           This name is passed to GENIE_MCA::INIT().
;
; OUTPUTS:
;       None (but see GENIE_MCA::INIT)
;
; RESTRICTIONS:
;       This routine cannot be called directly.  It is called indirectly when
;       creating a new object of class GENIE_MCA by the IDL OBJ_NEW()
;       function.
;
; EXAMPLE:
;       mca = obj_new('GENIE_MCA', 'DET01')
;       if (OBJ_VALID(mca)) then print, 'It worked!'
;
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, February 23, 1999
;-
    genie_mca = { genie_mca, $
            dll_name:      '', $   ; The name of the DLL which IDL calls
            detector_name: '', $   ; The name of the Genie detector
            hDSC:          0L, $   ; A pointer used by the Genie2k (S560) routines
            INHERITS mca}
end



;*****************************************************************************
pro mca::read_file, file
;+
; NAME:
;       MCA::READ_FILE
;
; PURPOSE:
;       This procedure reads a disk file into an MCA object.
;       This version replaces the one in MCA__DEFINE.  In order to use this
;       this file (genie_mca__define.pro) must be compiled AFTER mca__define.
;       pro.  This file first attempts to read a Canberra CAM file, and if that
;       fails, then it tries to open the file with MCA::READ_STANDARD_FILE
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       mca->READ_FILE, File
;
; INPUTS:
;       File:  The name of the disk file to read.
;
; OUTPUTS:
;       None
;
; EXAMPLE:
;       mca = obj_new('MCA')
;       mca->read_file, 'mca.001'
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, 10-FEB-2000
;-

;   Try to open a CAM configuration with this file name
    cam_file = obj_new('genie_mca', file)
    if (obj_valid(cam_file)) then begin
        data = cam_file->get_data()
        elapsed = cam_file->get_elapsed()
        calibration = cam_file->get_calibration()
        rois = cam_file->get_rois(roi_info)
        self.name = file
        self.nchans = n_elements(data[*,0])
        self->set_elapsed, elapsed[0]
        self->set_calibration, calibration[0]
        nrois = roi_info[0].nrois
        if (nrois gt 0) then begin
            self->set_rois, rois[0:nrois-1, 0]
        endif else begin
            self->set_rois
        endelse
        if (n_elements(environment) gt 0) then self->set_environment, environment
        self->set_data, data[*,0]
        obj_destroy, cam_file
    endif else begin
        self->mca::read_standard_file, file
    endelse
end

