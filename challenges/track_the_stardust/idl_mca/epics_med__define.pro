;*****************************************************************************
;+
; NAME:
; Release notes
;   Version 3:  
;       Everything prior to 12/1/99 when I started making release notes!  
;       Prior to this date there are release notes for each routine but no
;       global release notes for the EPICS_MCA class, and there are no 
;       version numbers
;   Version 4.0
;        Dec. 2, 1999. Mark Rivers
;           Changes to support new simplified MED databases. There is no longer
;           a Start PV which can both start and stop and automatically change
;           its value.  That was too complex and too unreliable.  Rather now
;           there is StartAll, EraseAll and EraseStart which simply does
;           EraseAll followed by StartAll.
;        Dec 2, 1999.  Mark Rivers
;           Added START keyword to ACQUIRE_WAIT.  Added Dwell, ChannelAdvance
;           and Prescale to presets. Fixed bugs in SET_PRESETS.
;        Dec 3, 1999.  Mark Rivers
;           Changed IdlBusy to ClientWait.  Changed logic in spectra_scan.
;           Added support for environment files as in EPICS_MCA objects.
;        October 26. 2000.  Mark Rivers
;           Improved example documentation for ::INIT and ::DEFINE
;-
;

;*****************************************************************************
pro epics_med::set_presets, presets
;+
; NAME:
;      EPICS_MED::SET_PRESETS
;
; PURPOSE:
;       This procedure sets the preset parameters for the MED.
;       The preset information is contained in a structure of type
;       MCA_PRESETS.
;
; CATEGORY:
;       EPICS IDL device class library.
;
; CALLING SEQUENCE:
;       epics_med->SET_PRESETS, Presets
;
; PROCEDURE:
;       This function knows about the EPICS database which fans out a single 
;       preset to each multiplexed group of detectors.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::SET_PRESETS">MCA::SET_PRESETS</A>.  
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 27, 1998
;       Dec. 1, 1999    Mark Rivers. Added .dwell, .channel_advance and .
;                       prescale.  Added [0] array index, required.
;-
    self->mca::set_presets, presets[0]
    t = caput( self.pvs.preal,  self.presets[0].real_time)
    t = caput( self.pvs.plive,  self.presets[0].live_time)
    t = caput( self.pvs.dwell,  self.presets[0].dwell)
    t = caput( self.pvs.channel_advance,  self.presets[0].channel_advance)
    t = caput( self.pvs.prescale,  self.presets[0].prescale)
end


;*****************************************************************************
pro epics_med::copy_rois, detector, energy=energy
;+
; NAME:
;      EPICS_MED::COPY_ROIS
;
; PURPOSE:
;       This procedure copies the ROIs defined for one detector to all of the
;       other detectors
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;       med->COPY_ROIS, Detector
;
; PROCEDURE:
;       This function simply converts from detector numbers as seen by the used
;       (1-N, including bad elements) to the index in the MCA object array in
;       the MED.  It then calls MED::COPY_ROIS.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::SET_ROIS">MCA::SET_ROIS</A>.  
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 25, 1998
;-
    if (n_elements(detector) eq 0) then detector=1
    detector = where(self.good_detectors eq detector, count) + 1
    if (count lt 0) then message, 'Invalid detector in COPY_ROIS'
    self->med::copy_rois, detector, energy=energy
end


;*****************************************************************************
function epics_med::get_acquire_status, update=update
;+
; NAME:
;       EPICS_MED::GET_ACQUIRE_STATUS
;
; PURPOSE:
;       This function returns the acquisition status for the MED.
;       This is 1 if the MED is acquiring and 0 if it is not acquiring.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;       Result = epics_med->GET_ACQUIRE_STATUS()
;
; KEYWORD PARAMETERS:
;	UPDATE:	Set this keyword to update the acquisition status.
;		By default this routine does NOT do this.
;
; PROCEDURE:
;       This function reads the acquisition status from the hardware using
;       the EPICS database.
;
;-
    if (keyword_set(update)) then t = caput(self.pvs.read, 1)
    t = caget(self.pvs.acquiring, acquiring)
    self.acquiring = acquiring
    return, self->mca::get_acquire_status()
end

;*****************************************************************************
function epics_med::get_environment, count, name=name, description=description
;+
; NAME:
;       EPICS_MED::GET_ENVIRONMENT
;
; PURPOSE:
;       This function gets the environment parameters for the MED.
;       The environment information is contained in an array of structures 
;       of type MCA_ENVIRONMENT.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       Result = epics_med->GET_ENVIRONMENT()
;
; PROCEDURE:
;       This function simply returns EPICS_MCA::GET_ENVIRONMENT for the
;       for EPICS_MCA in the EPICS_MED.
;
; KEYWORD PARAMETERS:
;       This function accepts all of the keyword paramters used by 
;       MCA::GET_ENVIRONMENT
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::GET_ENVIRONMENT">MCA::GET_ENVIRONMENT()</A>.
;
; EXAMPLE:
;       med = obj_new('EPICS_MED', '13GE1:med:')
;       env = med->GET_ENVIRONMENT(count)
;       ; Get all environment variables
;       help, /structure, env[0]
;       ; Get all of the sample information
;       env = med->GET_ENVIRONMENT(DESCRIPTION='Sample', count)
;       for i=0, count-1 do print, env[i].name, '=', env[i].value
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 29, 2001.  Put this code here
;                       and deleted from the MCA::WRITE_FILE routines.
;-
    return, self.mca_objs[0]->get_environment(count, $
                                       name=name, description=description)
end
    
;*****************************************************************************
pro epics_med::acquire_wait, dwell_time, start=start, stop=stop
;+
; NAME:
;       EPICS_MED::ACQUIRE_WAIT
;
; PURPOSE:
;       This procedures waits for acquisition of the detector array to complete.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;
;       epics_med->ACQUIRE_WAIT, Time
;
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
; OPTIONAL INPUTS:
;       Time:  The estimated acquisition time of the MED.
;
; PROCEDURE:
;       This routine simply polls to see if acquisition is complete using
;       EPICS_MED::GET_ACQUIRE_STATUS(). If the optional Time input is
;       specified then the time between polling is Time/10.  The default and
;       minimum time between polling is one second.
;
; EXAMPLE:
;       med = obj_new('EPICS_MED', '13IDC:med1')
;       med->ACQUIRE_ON
;       med->ACQUIRE_WAIT
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, February 13, 1998
;       Dec. 2, 1999    Added START keyword
;       MLR 18-Nov-2003 Added STOP keyword
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
pro epics_med::erase
;+
; NAME:
;       EPICS_MED::ERASE
;
; PURPOSE:
;       This procedures erases the array data, i.e. sets all channels of
;       each detector to zero.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;       epics_med->ERASE
;
; PROCEDURE:
;       This procedure erases the MED by sending the appropriate command to
;       the EPICS database.  For efficiency it does not call EPICS_MCA::ERASE
;       for each EPICS_MCA in the EPICS_MED.
;
; EXAMPLE:
;       med = obj_new('EPICS_MED', '13IDC:med1')
;       med->ERASE
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, October 1, 1997
;-
    t = caput(self.pvs.erase, 1)
    ; Set the acquire start time in case acquisition is on
    self.elapsed.start_time = systime() 
end


;*****************************************************************************
pro epics_med::acquire_on, erase=erase
;+
; NAME:
;       EPICS_MED::ACQUIRE_ON
;
; PURPOSE:
;       This procedure turns on acquisition of the detector array.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;       epics_med->ACQUIRE_ON
;
; KEYWORD PARAMETERS:
;	ERASE:	Set this keyword to erase the data before starting acquisition
;
; PROCEDURE:
;       This procedure starts the MED by sending the appropriate command to
;       the EPICS database.  For efficiency it does not call 
;       EPICS_MCA::ACQUIRE_ON for each EPICS_MCA in the EPICS_MED.
;
; EXAMPLE:
;       med = obj_new('EPICS_MED', '13IDC:med1')
;       med->ACQUIRE_ON
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, February 13, 1998
;-
    if (keyword_set(erase)) then begin
        t = caput(self.pvs.erasestart, 1)
    endif else begin
        t = caput(self.pvs.start, 1)
    endelse
    self.elapsed.start_time = systime() 
        ; This should really be done by the record
end


;*****************************************************************************
pro epics_med::acquire_off
;+
; NAME:
;       EPICS_MED::ACQUIRE_OFF
;
; PURPOSE:
;       This procedures turns off acquisition of the detector array.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;       epics_med->ACQUIRE_OFF
;
; EXAMPLE:
;       med = obj_new('EPICS_MED', '13IDC:med1')
;       med->ACQUIRE_OFF
;
; PROCEDURE:
;       This procedure stops the MED by sending the appropriate command to
;       the EPICS database.  For efficiency it does not call 
;       EPICS_MCA::ACQUIRE_OFF for each EPICS_MCA in the EPICS_MED.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, February 13, 1998
;       Dec. 1, 1999  MLR changed to write 0 to .start rather than 1 to .stop
;       Apr. 5, 2000  MLR changed back, write 1 to .stop rather than 0 to
;                     .start
;-
    t = caput(self.pvs.stop, 1)
end


;*****************************************************************************
function epics_med::get_data, total=total, align=align
;+
; NAME:
;       EPICS_MED::GET_DATA
;
; PURPOSE:
;       This function returns the data from the each MCA in the EPICS_MED.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       Result = med->GET_DATA(/TOTAL, /ALIGN)
;
; PROCEDURE:
;       This function simply determines the current number of channels in
;       the data (since NUSE might have changed) and then calls MED::GET_DATA.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="med_class.html#MED::GET_DATA">MED::GET_DATA()</A> for
;       and explanation of the keywords and the return value.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, December 3, 1999
;-
    nuse  = 2048
    t = caget(self.pvs.nuse, nuse)
    if (t eq 0) then self.nchans = nuse
    return, self->med::get_data(total=total, align=align)
end


;*****************************************************************************
pro epics_med::spectra_scan, first_file, scan_record
;+
; NAME:
;       EPICS_MED::SPECTRA_SCAN
;
; PURPOSE:
;       This procedures collects MED spectra in conjunction with an EPICS
;       scan record.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;       epics_med->SPECTRA_SCAN, First_file, Scan_record
;
; INPUTS:
;       First_file:  
;           The name of the first spectrum file to save.  Subsequent files 
;           will be named using the INCREMENT_FILENAME()function.  The 
;           filename must end in a numeric extension for this to work.
;
;       Scan_record:
;           The name of the EPIC scan record which is controlling the scan.
;           This scan record must start MED data collection by writing "1" 
;           into the EraseStart record of the MED database.
;       
; EXAMPLE:
;       med = obj_new('EPICS_MED', '13IDC:med1')
;       med->SPECTRA_SCAN, 'test.001', '13IDC:scan1'
;
; PROCEDURE:
;       1) Wait for scan.EXSC = 1, meaning scan has started
;       2) Wait for ClientWait=1, meaning acquisition has started
;       3) Wait for Acquiring=0, meaning acquisition has completed
;       4) Write data to disk with MED::WRITE_FILE, increment file name
;       5) Reset ClientWait to 0 so scan will continue
;       6) If scan.EXSC is still 1 go to 2.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, December 30, 1998
;       Dec. 3, 1999    Mark Rivers.  Changed record names, logic for new
;                       database.
;-
    file = first_file

;   Enable waiting for client
    t = caput(self.pvs.enable_client_wait, 1)

;   Wait for scan to start
    repeat t = caget(scan_record + '.EXSC', started) until (started)

    while (1) do begin
        ; If scan is complete, exit
        t = caget(scan_record + '.EXSC', scan_status)
        if (scan_status eq 0) then return

        ; Wait for acquisition to start
        repeat t = caget(self.pvs.client_wait, started) until (started)

        ; Wait for acquisition to complete
        self->acquire_wait

        ; Write file
        self->write_file, file
        print, 'Saved file: ', file
        file = increment_filename(file)

        ; Reset the client wait flag
        t = caput(self.pvs.client_wait, 0)  
    endwhile
end


;*****************************************************************************
function epics_med::init, prefix, n_detectors, bad_detectors=bad_detectors, $
                  environment_file=environment_file
;+
; NAME:
;       EPICS_MED::INIT
;
; PURPOSE:
;       This is the initialization code which is invoked when a new object of
;       type EPICS_MED is created.  It cannot be called directly, but only
;       indirectly by the IDL OBJ_NEW() function.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;       Result = OBJ_NEW('EPICS_MED', Prefix, N_detectors, BAD=bad)
;
; INPUTS:
;       Prefix:       The prefix of the EPICS process variables for this
;                     multi-element detector database.  The records for the 
;                     process variables must be named according to the
;                     following rules:
;       prefix + 'Start'  ; PV which starts acquisition when 1 is written to it
;       prefix + 'Stop'   ; PV which stops acquisition when 1 is written to it
;       prefix + 'EraseAll' ; PV which erases all MCAs when 1 is written to it
;       prefix + 'ReadSeq' ; PV which reads all MCAs when 1 is written to it
;       prefix + 'ElapsedLive' ; PV from which elapsed live time can be read
;       prefix + 'ElapsedReal' ; PV from which elapsed real time can be read
;       prefix + 'PresetLive'  ; PV to which preset live time can be written
;       prefix + 'PresetReal'  ; PV to which preset real time can be written
;       prefix + 'Acquiring'   ; PV which is 1 when any detector is acquiring,
;                              ;   0 when they are all done acquiring
;       prefix + 'mcaN'        ; Name of MCA record for each detector, e.g. 
;                              ; prefix + 'mca1', prefix + 'mca2', etc.
;
; OPTIONAL INPUTS:
;       N_detectors: The number of detectors in the MED.  Default is 16.
;
; KEYWORD PARAMETERS
;       BAD:    A scalar or array listing the bad detectors, e.g. BAD=[3,7].
;               The detectors are numbered from 1 to N_detectors.
;               These detectors will not be accessed by any of the MED methods.
;               In the following example:
;               med = obj_new('EPICS_MED', '13IDC:med:', 16, bad=[3,7])
;               detectors 3 and 7, out of a total of 16, are bad.  All of the
;               MED functions, such as GET_CALIBRATION(), GET_DATA(), etc.
;               will return only 14 values, not 16.
;
; OUTPUTS:
;       This function returns a status to indicate whether it was able to
;       establish channel access communication with the specified EPICS
;       records.  This status is 1 for success, 0 for failure.  This status is
;       passed back indirectly to the routine which calls OBJ_NEW().  OBJ_NEW
;       will return a valid object pointer if this routine succeeds, and will
;       return a NULL object pointer if this routine fails.  The user should
;       test the return value of OBJ_NEW() with the IDL function OBJ_VALID().
;
; SIDE EFFECTS:
;       The routine establishes channel access monitors on all of the fields
;       in the records which the methods in this class will read.  This
;       greatly improves the speed and efficiency.
;
; RESTRICTIONS:
;       This routine cannot be called directly.  It is called indirectly when
;       creating a new object of class EPICS_MED by the IDL OBJ_NEW()
;       function.
;
; EXAMPLE:
;       ; Create an EPICS_MED object for a 13 element detector, with elements 3 and 7
;       ; bad.
;       med = obj_new('EPICS_MED', '13IDC:med:', 13, bad=[3,7])
;       if (OBJ_VALID(med)) then print, 'It worked!'
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, February 4, 1998
;       17-Sep-1998 MLR  Removed caStartGroup, caEndGroup, since these don't
;                        work with the non-callback version of caput, which
;                        we are now using (ezcaPutOldCa)
;       26-Oct-2000 MLR  Improved example documentation 
;-
    if (n_elements(n_detectors) eq 0) then n_detectors=16
    if (n_elements(environment_file) eq 0) then environment_file = 'catch1d.env'

    t = self->med::init(n_detectors)  ; Invoke base class initialization
    self.pvs.start = prefix + 'StartAll'
    self.pvs.erasestart = prefix + 'EraseStart'
    self.pvs.stop  = prefix + 'StopAll'
    self.pvs.erase = prefix + 'EraseAll'
    self.pvs.read  = prefix + 'ReadAll'
    self.pvs.nuse  = prefix + 'NuseAll'
    self.pvs.elive  = prefix + 'ElapsedLive'
    self.pvs.ereal  = prefix + 'ElapsedReal'
    self.pvs.plive  = prefix + 'PresetLive'
    self.pvs.preal  = prefix + 'PresetReal'
    self.pvs.dwell  = prefix + 'Dwell'
    self.pvs.channel_advance  = prefix + 'ChannelAdvance'
    self.pvs.prescale  = prefix + 'Prescale'
    self.pvs.acquiring  = prefix + 'Acquiring'
    self.pvs.client_wait  = prefix + 'ClientWait'
    self.pvs.enable_client_wait  = prefix + 'EnableClientWait'
    good_detectors = indgen(self.n_detectors) + 1
    if (n_elements(bad_detectors) ne 0) then begin
        good_detectors[bad_detectors-1] = -1
        good_detectors = good_detectors[where(good_detectors ge 0)]
    endif
    self.n_detectors = n_elements(good_detectors)
    self.good_detectors = good_detectors
    for i=0, self.n_detectors-1 do begin
        pv = prefix + 'mca' + strtrim(self.good_detectors[i], 2)
        self.mca_objs[i] = obj_new('epics_mca', pv,environment_file=environment_file)
        if obj_valid(self.mca_objs[i]) ne 1 then return, 0
    endfor
    t = casetmonitor(self.pvs.elive)
    if (t ne 0) then return, 0
    t = casetmonitor(self.pvs.ereal)
    if (t ne 0) then return, 0
    t = casetmonitor(self.pvs.acquiring)
    if (t ne 0) then return, 0
    t = casetmonitor(self.pvs.client_wait)
    if (t ne 0) then return, 0
    ; Read the first MCA to get the number of channels
    t = self.mca_objs[0]->get_data()
    self.nchans = n_elements(t)
    ; Read the environment from the first MCA
    env = self.mca_objs[0]->get_environment(count)
    if (count gt 0) then self.environment = ptr_new(env, /no_copy)
    return, 1
end


;*****************************************************************************
pro epics_med__define
;+
; NAME:
;       EPICS_MED__DEFINE
;
; PURPOSE:
;       This is the definition code which is invoked when a new object of
;       type EPICS_MED is created.  It cannot be called directly, but only
;       indirectly by the IDL OBJ_NEW() function,
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;       Result = OBJ_NEW('EPICS_MED', Prefix)
;
; INPUTS:
;       Prefix:       The prefix of the EPICS process variables for this
;                     dectector.  The records for the detector must be named
;                     according to fixed rules.
;
; OUTPUTS:
;       None (but see EPICS_MCA::INIT)
;
; RESTRICTIONS:
;       This routine cannot be called directly.  It is called indirectly when
;       creating a new object of class EPICS_MED by the IDL OBJ_NEW()
;       function.
;
; EXAMPLE:
;       ; Create an EPICS_MED object for a 13 element detector, with elements 3 and 7
;       ; bad.
;       med = obj_new('EPICS_MED', '13IDC:med:', 13, bad=[3,7])
;       if (OBJ_VALID(med)) then print, 'It worked!'
;
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, February 4, 1998
;       26-Oct-2000 MLR  Improved example documentation 
;-
    MAX_DETECTORS=30
    epics_med = $
          { epics_med, $
            pvs: { epics_med_pvs, $
                start: "", $
                erasestart: "", $
                stop: "",  $
                erase: "", $
                read: "",  $
                acquiring: "", $
                client_wait: "", $
                nuse: "", $
                enable_client_wait: "", $
                plive: "", $
                preal: "", $
                dwell: "", $
                channel_advance: "", $
                prescale: "", $
                elive: "", $
                ereal: ""  $
            }, $
            good_detectors: intarr(MAX_DETECTORS), $
            INHERITS med $
          }
end
