;*****************************************************************************
pro med::initial_calibration, energy
;+
; NAME:
;       MED::INITIAL_CALIBRATION
;
; PURPOSE:
;       This procedure does an initial energy calibration for each MCA in the
;       MED.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       med->INITIAL_CALIBRATION, Energy
;
; INPUTS:
;       Energy:  The energy of the largest peak in the spectrum.
;
; PROCEDURE:
;       This function simply invokes MCA::INITIAL_CALIBRATION for each MCA in
;       the MED.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::INITIAL_CALIBRATION">MCA::INITIAL_CALIBRATION</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 25, 1998
;       04-APR-2000 MLR Read data before doing calibration
;-
    junk = self->get_data()
    for i=0, self.n_detectors-1 do begin
        self.mca_objs[i]->initial_calibration, energy
    endfor
end


;*****************************************************************************
pro med::final_calibration, peaks
;+
; NAME:
;       MED::FINAL_CALIBRATION
;
; PURPOSE:
;       This procedure does a final energy calibration for each MCA in the
;       MED.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       med->FINAL_CALIBRATION, Peaks
;
; INPUTS:
;       Peaks:  An array of structures of type MCA_PEAKS containing the
;               information on the peaks to be fit.  This array is typically
;               read from a disk file with function READ_PEAKS().
;
; PROCEDURE:
;       This function simply invokes MCA::FINAL_CALIBRATION for each MCA in
;       the MED.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::FINAL_CALIBRATION">MCA::FINAL_CALIBRATION</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 25, 1998
;-
    for i=0, self.n_detectors-1 do begin
        self.mca_objs[i]->final_calibration, peaks
    endfor
end


;*****************************************************************************
pro med::set_calibration, calibration
;+
; NAME:
;       MED::SET_CALIBRATION
;
; PURPOSE:
;       This procedure sets the calibration parameters for the MED.
;       The calibration information is contained in a structure or an array
;       of structures of type MCA_CALIBRATION.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       med->SET_CALIBRATION, Calibration
;
; INPUTS:
;       Calibration:  A single structure or an array of structures of type
;                     MCA_CALIBRATION containing the calibration parameters
;                     for each MCA.  If a single structure is passed then this
;                     is written to each MCA.  If an array of structures is
;                     passed then Calibration[i] is written to MCA[i].
;
; PROCEDURE:
;       This function simply invokes MCA::SET_CALIBRATION for each MCA in the
;       MED.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::SET_CALIBRATION">MCA::SET_CALIBRATION</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 25, 1998
;-
    if n_elements(calibration) eq 1 then begin
        cal = replicate(calibration, self.n_detectors)
    endif else begin
        cal = calibration
    endelse
    for i=0, n_elements(cal)-1 do begin
        self.mca_objs[i]->set_calibration, cal[i]
    endfor
end


;*****************************************************************************
function med::get_energy
;+
; NAME:
;       MED::GET_ENERGY
;
; PURPOSE:
;       This function returns a 2-D array containing the energy scale for
;       each MCA in the MED.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       Result = med->GET_ENERGY()
;
; OUTPUTS:
;       This function returns a 2-D float array, dimensioned
;       [self.nchans, self.n_detectors]
;
; PROCEDURE:
;       This function simply invokes MCA::GET_ENERGY for each MCA in the
;       MED and stores the results in the returned array.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::GET_ENERGY">MCA::GET_ENERGY()</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 27, 1998
;-
    energy = fltarr(self.nchans, self.n_detectors)
    for i=0, self.n_detectors-1 do begin
        energy[0,i] = self.mca_objs[i]->get_energy()
    endfor
    return, energy
end


;*****************************************************************************
function med::get_calibration
;+
; NAME:
;       MED::GET_CALIBRATION
;
; PURPOSE:
;       This function returns the calibration parameters for the MED.
;       The calibration information is contained in an array of structures
;       of type MCA_CALIBRATION.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       Result = med->GET_CALIBRATION()
;
; OUTPUTS:
;       This function returns an array of structures of type MCA_CALIBRATION.
;
; PROCEDURE:
;       This function simply invokes MCA::GET_CALIBRATION for each MCA in the
;       MED and stores the results in the returned array.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::GET_CALIBRATION">MCA::GET_CALIBRATION()</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 25, 1998
;-
    calibration = replicate({MCA_CALIBRATION}, self.n_detectors)
    for i=0, self.n_detectors-1 do begin
        calibration[i] = self.mca_objs[i]->get_calibration()
    endfor
    return, calibration
end


;*****************************************************************************
function med::get_mca_objects, detector
;+
; NAME:
;       MED::GET_MCA_OBJECTS
;
; PURPOSE:
;       This function returns the object pointers to the MCA objects in the
;       MED.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       Result = med->GET_MCA_OBJECTS()
;
; OUTPUTS:
;       This function returns an array of object pointers to the MCA objects
;       in the MED.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 27, 1998
;-
    return, self.mca_objs[0:self.n_detectors-1]
end


;*****************************************************************************
function med::get_elapsed
;+
; NAME:
;       MED::GET_ELAPSED
;
; PURPOSE:
;       This function returns the elapsed parameters for the MED.
;       The elapsed information is contained in an array of structures of type
;       MCA_ELAPSED.
;
; CATEGORY:
;       EPICS device class library.
;
; CALLING SEQUENCE:
;       Result = med->GET_ELAPSED()
;
; OUTPUTS:
;       This function returns an array of structures of type MCA_ELAPSED.
;
; PROCEDURE:
;       This function simply invokes MCA::GET_ELAPSED for each MCA in the MED
;       and stores the results in the returned array.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::GET_ELAPSED">MCA::GET_ELAPSED()</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 25, 1998
;-
    elapsed = replicate({MCA_ELAPSED}, self.n_detectors)
    for i=0, self.n_detectors-1 do begin
        elapsed[i] = self.mca_objs[i]->get_elapsed()
    endfor
    return, elapsed
end


;*****************************************************************************
pro med::set_elapsed, elapsed
;+
; NAME:
;       MED::SET_ELAPSED
;
; PURPOSE:
;       This procedure set the elapsed parameters for the MED.
;       The elapsed information is contained in a structure or an array of
;       structures of type MCA_ELAPSED.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       med->SET_ELAPSED(), Elapsed
;
; INPUTS:
;       Elapsed:  A single structure or an array of structures of type
;                 MCA_ELAPSED containing the elapsed parameters for each MCA.
;                 If a single structure is passed then this is written to
;                 each MCA.  If an array of structures is passed then
;                 Elapsed[i] is written to MCA[i].
;
; PROCEDURE:
;       This function simply invokes MCA::SET_ELAPSED for each MCA in the MED.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::SET_ELAPSED">MCA::SET_ELAPSED()</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 25, 1998
;-
    if n_elements(elapsed) eq 1 then begin
        el = replicate(elapsed, self.n_detectors)
    endif else begin
        el = elapsed
    endelse
    for i=0, n_elements(el)-1 do begin
        self.mca_objs[i]->set_elapsed, el[i]
    endfor
end


;*****************************************************************************
function med::get_presets
;+
; NAME:
;       MED::GET_PRESETS
;
; PURPOSE:
;       This function returns the preset parameters for each MCA in the MED.
;       The preset information is contained in an array of structures of type
;       MCA_PRESETS.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       Result = med->GET_PRESETS()
;
; PROCEDURE:
;       This function simply invokes MCA::GET_PRESETS for each MCA in the MED
;       and stores the output in the returned array.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::GET_PRESETS">MCA::GET_PRESETS()</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 25, 1998
;-
    presets = replicate({MCA_PRESETS}, self.n_detectors)
    for i=0, self.n_detectors-1 do begin
        presets[i] = self.mca_objs[i]->get_presets()
    endfor
    return, presets
end


;*****************************************************************************
pro med::set_presets, presets
;+
; NAME:
;      MED::SET_PRESETS
;
; PURPOSE:
;       This procedure sets the preset parameters for each MCA in the MED.
;       The preset information is contained in a structure or array of
;       structure of type MCA_PRESETS.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       med->SET_PRESETS, Presets
;
; INPUTS:
;       Presets:  A single structure or an array of structures of type
;                 MCA_PRESETS containing the preset parameters for each MCA.
;                 If a single structure is passed then this is written to
;                 each MCA.  If an array of structures is passed then
;                 Presets[i] is written to MCA[i].
;
;
; PROCEDURE:
;       This function simply invokes MCA::SET_PRESETS for each MCA in the MED.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::SET_PRESETS">MCA::SET_PRESETS</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 28, 1998
;-
    if n_elements(presets) eq 1 then begin
        p = replicate(presets, self.n_detectors)
    endif else begin
        p = presets
    endelse
    for i=0, n_elements(p)-1 do begin
        self->mca::set_presets, p[i]
    endfor
end


;*****************************************************************************
function med::get_sequence
;+
; NAME:
;       MED::GET_SEQUENCE
;
; PURPOSE:
;       This function returns the current sequence number for each MCA in the
;       MED.  Sequences are used for time resolved spectroscopy, and refer to
;       different regions of MCA memory for data acquistion and readout.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       Result = med->GET_SEQUENCE()
;
; OUTPUTS:
;       This function returns a long array containing the current sequence
;       number of each MCA in the MED.
;
; PROCEDURE:
;       This function simply invokes MCA::GET_SEQUENCE for each MCA in the MED
;       and stores the result in the returned array.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::GET_SEQUENCE">MCA::GET_SEQUENCE</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 25, 1998
;-
    sequence = lonarr(self.n_detectors)
    for i=0, self.n_detectors-1 do begin
        sequence[i] = self.mca_objs->get_sequence()
    endfor
    return, sequence
end


;*****************************************************************************
pro med::set_sequence, sequence
;+
; NAME:
;      MED::SET_SEQUENCE
;
; PURPOSE:
;       This procedure sets the current sequence number for each MCA in the
;       MED.  Sequences are used for time resolved spectroscopy, and refer to
;       different regions of MCA memory for data acquistion and readout.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       med->SET_SEQUENCE, Sequence
;
; INPUTS:
;       Sequence: A scalar or array of sequence numbers.
;                 If a single sequence number is passed then this is written to
;                 each MCA.  If an array of sequences is passed then
;                 Sequence[i] is written to MCA[i].
;
; PROCEDURE:
;       This function simply invokes MCA::SET_SEQUENCE for each MCA in the MED.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::SET_SEQUENCE">MCA::SET_SEQUENCE</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 25, 1998
;-
    if n_elements(sequence) eq 1 then begin
        s = replicate(sequence, self.n_detectors)
    endif else begin
        s = sequence
    endelse
    for i=0, n_elements(s)-1 do begin
        self.mca_objs[i]->set_sequence, s[i]
    endfor
end


;*****************************************************************************
function med::get_rois, roi_info, energy=energy
;+
; NAME:
;       MED::GET_ROIS
;
; PURPOSE:
;       This function returns the region-of-interest information for each MCA
;       in the MED. The ROI information is contained in an array of structures
;       of type MCA_ROI.  Additional information about the number of ROIs
;       defined for each MCA is returned in an array of structures of type
;       MCA_ROI_INFO.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       Result = med->GET_ROIS(Roi_Info, /ENERGY)
;
; OUTPUTS:
;       This function returns a 2-D array of structures of type MCA_ROI.  The
;       dimensions of this array are [self.n_detectors, self.MAX_ROIS].
;
;       Roi_Info: This optional output is an array of structures of type
;                 MCA_ROI_INFO which contains information on the number of
;                 rois defined for each MCA.
;
; PROCEDURE:
;       This function simply invokes MCA::GET_ROIS for each MCA in the MED
;       and stores the results in the returned arrays.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::GET_ROIS">MCA::GET_ROIS()</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 25, 1998
;-
    rois = replicate({MCA_ROI}, self.MAX_ROIS, self.n_detectors)
    roi_info = replicate({MCA_ROI_INFO}, self.n_detectors)
    for i=0, self.n_detectors-1 do begin
        rois[0,i]=self.mca_objs[i]->get_rois(temp, energy=energy)
        roi_info[i] = temp
    endfor
    self.nrois = max(roi_info.nrois)
    return, rois[0: (self.nrois-1)>0, *]
end


;*****************************************************************************
pro med::get_roi_counts, total, net
;+
; NAME:
;       MED::GET_ROI_COUNTS
;
; PURPOSE:
;       This procedures returns the net and total counts for each ROI in each
;       MCA in the MED.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       med->GET_ROI_COUNTS, Total, Net
;
; OUTPUTS:
;       Total:  A 2-D array dimensioned [self.MAX_ROIS, self.n_detectors]
;               containing the total counts in each ROI for each MCA.
;       Net:    A 2-D array dimensioned [self.MAX_ROIS, self.n_detectors]
;               containing the net counts in each ROI for each MCA.
;
; PROCEDURE:
;       This function simply invokes MCA::GET_ROI_COUNTS for each MCA in the MED
;       and stores the results in the returned arrays.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::GET_ROI_COUNTS">MCA::GET_ROI_COUNTS()</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 25, 1998
;-
    total = fltarr(self.MAX_ROIS, self.n_detectors)
    net =   fltarr(self.MAX_ROIS, self.n_detectors)
    for i=0, self.n_detectors-1 do begin
        self.mca_objs[i]->get_roi_counts, total_temp, net_temp
        total[0, i] = total_temp
        net[0, i] = net_temp
    endfor
end


;*****************************************************************************
pro med::set_rois, rois, energy=energy
;+
; NAME:
;      MED::SET_ROIS
;
; PURPOSE:
;       This procedure sets the region-of-interest information for each MCA
;       in the MED.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       med->SET_ROIS, Rois, /ENERGY
;
; INPUTS:
;       Rois:  A 1-D or 2-D array of structures of type MCA_ROI.  If Rois is a
;              1-D array then the same ROIs are written to each MCA in the
;              MED.  If Rois is a 2-D array then Rois[*,i] is written to
;              detector i. If this input parameter is missing or not defined
;              then all ROIs in each MCA are cleared.
;
; KEYWORD PARAMETERS:
;       ENERGY: Set this keyword if the .LEFT and .RIGHT fields in each ROI
;               are defined in energy rather than in channel numbers.  This
;               is very useful when defining ROIs when the calibration
;               parameters for each MCA in the MED are not identical.
;
; PROCEDURE:
;       This function simply invokes MCA::SET_ROIS for each MCA in the MED.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::SET_ROIS">MCA::SET_ROIS</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 25, 1998
;       18-NOV-1998 MLR Added support for Rois being 1-D or 2-D
;-
    dims = size(Rois, /n_dimensions)
    for i=0, self.n_detectors-1 do begin
        if (dims eq 2) then begin
            self.mca_objs[i]->set_rois, rois[*,i], energy=energy
        endif else begin
            self.mca_objs[i]->set_rois, rois, energy=energy
        endelse
    endfor
end


;*****************************************************************************
pro med::add_roi, roi, energy=energy
;+
; NAME:
;       MED::ADD_ROI
;
; PURPOSE:
;       This function adds a new region-of-interest to each MCA in the MED.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       med->ADD_ROI, Roi, /ENERGY
;
; INPUTS:
;       Roi:  A structure of type MCA_ROI which defines the ROI to be added.
;
; KEYWORD PARAMETERS:
;       ENERGY: Set this keyword if the .LEFT and .RIGHT fields in the ROI
;               are defined in energy rather than in channel numbers.  This
;               is very useful when defining ROIs when the calibration
;               parameters for each MCA in the MED are not identical.
;
; PROCEDURE:
;       This function simply invokes MCA::ADD_ROI for each MCA in the MED.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::ADD_ROI">MCA::ADD_ROI</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 25, 1998
;-
    for i=0, self.n_detectors-1 do begin
        status = self.mca_objs[i]->add_roi(roi, energy=energy)
    endfor
end


;*****************************************************************************
pro med::del_roi, index
;+
; NAME:
;       MED::DEL_ROI
;
; PURPOSE:
;       This function deletes a region-of-interest from each MCA in the MED.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       med->DEL_ROI, Index
;
; INPUTS:
;       Index:  The index of the ROI to delete in the range 0 to
;               self.MAX_ROIS-1
;
; PROCEDURE:
;       This function simply invokes MCA::DEL_ROI for each MCA in the MED.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::DEL_ROI">MCA::DEL_ROI</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 25, 1998
;-
    for i=0, self.n_detectors-1 do begin
        self.mca_objs[i]->del_roi, index
    endfor
end


;*****************************************************************************
pro med::copy_rois, detector, energy=energy
;+
; NAME:
;      MED::COPY_ROIS
;
; PURPOSE:
;       This procedure copies the ROIs defined for one MCA in the MED to all of
;       the other MCAs.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       med->COPY_ROIS, Detector, /ENERGY
;
; INPUTS:
;       Detector:  The number of the detector (MCA) from which the ROIs are to
;                  be copied.  This number ranges from 1 to self.n_detectors.
;                  The default is detector 1.
;
; KEYWORD PARAMETERS:
;       ENERGY: Set this keyword if the ROIs should be copied by their position
;               in energy rather than in channels. This is very useful when
;               copying ROIs when the calibration parameters for each MCA in
;               the MED are not identical.
;
; PROCEDURE:
;       This function invokes MCA::GET_ROIS on the specified detector
;       and then MED::SET_ROIS.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::GET_ROIS">MCA::SET_ROIS</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 25, 1998
;-


    if (n_elements(detector) eq 0) then det=0 else det = detector-1
;{mn 10-02-2003:  um, this is sort of weird, but det now needs to be
;          converted from array of longs to a long scalar
    det = det[0]
;}

;{mn  02-11-99 this seemed to help:
;     rois = self.mca_objs[det]->get_rois(energy=energy)
    rois = self.mca_objs[det]->get_rois(temp, energy=energy)
;}
    self->set_rois, rois, energy=energy
end


;*****************************************************************************
function med::get_data, total=total, align=align
;+
; NAME:
;       MED::GET_DATA
;
; PURPOSE:
;       This function returns the data from the each MCA in the MED.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       Result = med->GET_DATA(/TOTAL, /ALIGN)
;
; KEYWORD PARAMETERS:
;       TOTAL:  Set this keyword to return the sum of the spectra from all
;               of the MCAs.
;
;       ALIGN: Set this keyword to return spectra which have been shifted and
;               and stretched to match the energy calibration parameters of the
;               first detector.  This permits doing arithmetic on a
;               "channel-by-channel" basis. This keyword can be used alone
;               or together with the TOTAL keyword, in which case the data
;               are aligned before summing.
;
; OUTPUTS:
;       By default this function returns a long 2-D array of counts dimensioned
;       [self.nchans, self.n_detectors]
;
;       If the TOTAL keyword is set then the function returns a long 1-D array
;       dimensioned [self.nchans].
;
; PROCEDURE:
;       This function simply invokes MCA::GET_DATA for each MCA in the MED and
;       stores the results in the returned array.  If the ALIGN keyword is
;       specified then all spectra except the first are shifted and stretched
;       using IDL's POLY_2D procedure to match the energy calibration
;       parameters of the first detector.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::GET_DATA">MCA::GET_DATA()</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 25, 1998
;       Mark Rivers Nov. 24, 1998.  Added MIN_SLOPE to prevent divide by 0
;-
    MIN_SLOPE = 1.e-10
    data = lonarr(self.nchans, self.n_detectors)
    for i=0, self.n_detectors-1 do begin
        data[0,i] = self.mca_objs[i]->get_data()
    endfor
    if (keyword_set(align)) then begin
        cal = self->get_calibration()
        o1 = cal[0].offset
        s1 = cal[0].slope > MIN_SLOPE
        oc1 = o1/s1
        for i=1,self.n_detectors-1 do begin
            ; POLY_2D only works with 2-D data, fake it out
            dd = [[data[*,i]], [data[*,i]]]
            oc = cal[i].offset/(cal[i].slope > MIN_SLOPE)
            p = [oc1-oc, 0, s1/(cal[i].slope > MIN_SLOPE), 0]
            q = [0, 1., 0, 0]
            dd = poly_2d(dd, p, q)
            data[0,i] = dd[*,0]
        endfor
    endif
    if (keyword_set(total)) then begin
        d = total(data, 2)
        return, d
    endif
    return, data
end


;*****************************************************************************
pro med::set_data, data
;+
; NAME:
;       MED::SET_DATA
;
; PURPOSE:
;       This procedure writes data to each MCA in the MED
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       med->SET_DATA, Data
;
; INPUTS:
;       Data:
;          Data to be written.  Dimensions are either [NCHANS], in which case
;          the same data are written to each detector, or [NCHANS, NDETECTORS], in
;          which case Data[*,i] is written to detector i.
;
; PROCEDURE:
;       This procedure simply invokes MCA::SET_DATA for each MCA in the MED.
;
; ADDITIONAL INFORMATION:
;       See <A HREF="mca_class.html#MCA::SET_DATA">MCA::GET_DATA()</A>.
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Nov. 16, 2003
;-
    ndims = size(data, /n_dimensions)
    if (ndims eq 1) then begin
       for i=0, self.n_detectors-1 do begin
          self.mca_objs[i]->set_data, data
       endfor
    endif else begin
       for i=0, self.n_detectors-1 do begin
          self.mca_objs[i]->set_data, data[*,i]
       endfor
    endelse
end


;*****************************************************************************
pro med::read_file, file
;+
; NAME:
;       MED::READ_FILE
;
; PURPOSE:
;       This procedure reads a disk file into an MED object.  The file format
;       is a tagged ASCII format.  The file contains the information from the
;       MED object which it makes sense to store permanently, but does not
;       contain all of the internal state information for the MCA.  This
;       procedure reads files written with <A HREF="#MED::WRITE_FILE">MED::WRITE_FILE</A>.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;
;       med->READ_FILE, File
;
; INPUTS:
;       File:  The name of the disk file to read.
;
; OUTPUTS:
;       None
;
; EXAMPLE:
;       med = obj_new('MED')
;       med->read_file, 'med.001'
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 29, 1998
;       28-SEP-1998  MLR  Made common MCA_READ_FILE routine called by
;                         MED::READ_FILE and MCA::READ_FILE
;       18-NOV-1998  MLR  Added Environment to MCA_READ_FILE call
;       24-NOV-1998  MLR  Corrected logic when med has fewer detectors than
;                         the data file has.
;       21-SEP-2001  MLR  Added set_name for each mca.
;-
    temp = self->get_rois(roi_info)
    mca_read_file, file, elapsed, calibration, rois, roi_info, environment, data
    self.name = file
    nd = n_elements(elapsed)
    self.n_detectors = self.n_detectors < nd
    nd = self.n_detectors
    self.nchans = n_elements(data[*,0])
    elapsed = elapsed[0:nd-1]
    calibration = calibration[0:nd-1]
    if (n_elements(rois) gt 0) then rois = rois[*, 0:nd-1]
    roi_info = roi_info[0:nd-1]
    self->set_elapsed, elapsed
    self->set_calibration, calibration
    for i=0, self.n_detectors-1 do begin
        nrois = roi_info[i].nrois
        if (nrois gt 0) then begin
            self.mca_objs[i]->set_rois, rois[0:nrois-1, i]
        endif else begin
            self.mca_objs[i]->set_rois
        endelse
        self.mca_objs[i]->set_data, data[*,i]
        self.mca_objs[i]->set_name, self.name + ':' + strtrim(i+1, 2)
    endfor
    self->set_environment, environment
end


;*****************************************************************************
function med::init, n_detectors
;+
; NAME:
;       MED::INIT
;
; PURPOSE:
;       This is the initialization code which is invoked when a new object of
;       type MED is created.  It cannot be called directly, but only
;       indirectly by the IDL OBJ_NEW() function.
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       Result = OBJ_NEW('MED', N_detectors)
;
; INPUTS:
;       N_detectors:  The number of detectors (MCA objects) in this MED.
;                     The default is 13.
;
; OUTPUTS:
;       This function always returns a 1 to indicate that it succeeded.
;
; RESTRICTIONS:
;       This routine cannot be called directly.  It is called indirectly when
;       creating a new object of class MED by the IDL OBJ_NEW()
;       function.
;
; EXAMPLE:
;       med = obj_new('MED', 15)
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 25, 1998
;-
    MAX_DETECTORS = 30
    t = self->mca::init()  ; Invoke base class initialization
    self.max_detectors = MAX_DETECTORS
    if (n_elements(n_detectors) ne 0) then begin
        self.n_detectors = n_detectors
    endif else begin
        self.n_detectors = self.MAX_DETECTORS
    endelse
    for i=0, self.n_detectors-1 do begin
            self.mca_objs[i] = obj_new('mca')
    endfor
    return, 1
end


;*****************************************************************************
pro med__define
;+
; NAME:
;       MED__DEFINE
;
; PURPOSE:
;       This is the definition code which is invoked when a new object of
;       type MED is created.  It cannot be called directly, but only
;       indirectly by the IDL OBJ_NEW() function,
;
; CATEGORY:
;       IDL device class library.
;
; CALLING SEQUENCE:
;       Result = OBJ_NEW('MED', N_detectors)
;
; INPUTS:
;       N_detectors:  The number of detectors (MCA objects) in this MED.
;                     The default is 13.
;
; OUTPUTS:
;       None (but see MED::INIT)
;
; RESTRICTIONS:
;       This routine cannot be called directly.  It is called indirectly when
;       creating a new object of class MED by the IDL OBJ_NEW()
;       function.
;
; EXAMPLE:
;       med = obj_new('MED', 15)
;
; MODIFICATION HISTORY:
;       Written by:     Mark Rivers, Sept. 25, 1998
;-
    MAX_DETECTORS = 30
    med = $
          { med, $
            max_detectors:  0L, $
            n_detectors:    0L, $
            mca_objs: objarr(MAX_DETECTORS), $
            INHERITS mca $
          }
end
