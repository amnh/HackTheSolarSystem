pro popup_program_info_event, event

; Whenever this routine is called it is time to disappear - either the timer
; expired or the user pressed Dismiss
widget_control, event.top, /destroy
end

pro popup_program_info, title=title, text, time=time, font=font
if n_elements(title) eq 0 then title='Program release information'
if n_elements(time) eq 0 then time=5
base = widget_base(title = title, /column)
col = widget_base(base, /column, /frame)
for i=0, n_elements(text)-1 do begin
   t = widget_label(col, value=text(i), font=font)
endfor
t = widget_button(base, value='Dismiss', /align_center, font=font)
widget_control, base, /realize
widget_control, base, timer=time
xmanager, 'popup_program_info', base, /no_block
end
