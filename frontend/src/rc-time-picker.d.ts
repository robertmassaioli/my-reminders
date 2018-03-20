declare module 'rc-time-picker' {
    import * as moment from 'moment';
    
    interface TimepickerProps {
        prefixCls?: string; //	String	'rc-time-picker'	prefixCls of this component
        clearText?: string; //	String	'clear'	clear tooltip of icon
        disabled?: boolean; //	Boolean	false	whether picker is disabled
        allowEmpty?: boolean; //	Boolean	true	allow clearing text
        open?: boolean; //	Boolean	false	current open state of picker. controlled prop
        defaultValue?: moment.Moment; //	moment	null	default initial value
        defaultOpenValue?: moment.Moment; //	moment	moment()	default open panel value, used to set utcOffset,locale if value/defaultValue absent
        value?:	moment.Moment; //	null	current value
        placeholder?: string; //	String	''	time input's placeholder
        className?: string; //	String	''	time picker className
        popupClassName?: string; //	String	''	time panel className
        showHour?: boolean; //	Boolean	true	whether show hour
        showMinute?: boolean; //	Boolean	true	whether show minute
        showSecond?: boolean; //	Boolean	true	whether show second
        formatL?: string; //	String	-	moment format
        disabledHours?: () => number[]; //	Function	-	disabled hour options
        disabledMinutes?: () => number[]; //	Function	-	disabled minute options
        disabledSeconds?: () => number[]; //	Function	-	disabled second options
        use12Hours?: boolean; //	Boolean	false	12 hours display mode
        hideDisabledOptions?: boolean; //	Boolean	false	whether hide disabled options
        onChange?: (value: moment.Moment) => void; //	Function	null	called when select a different value
        // addon	Function	-	called from timepicker panel to render some addon to its bottom, like an OK button. Receives panel instance as parameter, to be able to close it like panel.close().
        placement?: 'left' | 'right' | 'top' | 'bottom' | 'topLeft' | 'topRight' | 'bottomLeft' | 'bottomRight';
        transitionName?: string; //	String	''	
        name?: string; //	String	-	sets the name of the generated input
        // onOpen	Function({ open })		when TimePicker panel is opened
        // onClose	Function({ open })		when TimePicker panel is opened
        hourStep?: number; //	Number	1	interval between hours in picker
        minuteStep?: number; //	Number	1	interval between minutes in picker
        secondStep?: number; //	Number	1	interval between seconds in picker
        focusOnOpen?: boolean; //	Boolean	false	automatically focus the input when the picker opens
        inputReadOnly?: boolean; //	Boolean	false	set input to read only
    }

    export default class TimePicker extends React.Component<TimepickerProps> {}
}