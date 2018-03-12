declare module '@atlaskit/button' {
    interface PropTypes {
        appearance?: "default" | "danger" | "link" | "primary" | "subtle" | "subtle-link" | "warning" | "help";
        ariaExpanded?: boolean;
        ariaHaspopup?: boolean;
        className?: string;
        isSelected?: boolean;
        isDisabled?: boolean;
        iconBefore?: React.ReactNode;
        iconAfter?: React.ReactNode;
        id?: string;
        onClick?(): void;
        spacing?: "compact" | "default" | "none";
        href?: string;
        shouldFitContainer?: boolean;
    }
    export default class Button extends React.Component<PropTypes> { }
    export class ButtonGroup extends React.Component {}
}

declare module '@atlaskit/droplist' {
    interface PropTypes {
        appearance?: 'default' | 'tall';
        boundariesElement?: 'viewport' | 'window' | 'scrollParent';
        isLoading?: boolean;
        isOpen?: boolean;
        onClick?(): void;
        onKeyDown?(): void;
        onOpenChange?(): void;
        shouldFitContainer?: boolean;
        shouldFlip?: boolean;
        maxHeight?: number;
        trigger?: JSX.Element;
        position?: string;
    }
    
    export default class DropList extends React.Component<PropTypes> { }

    export class ItemGroup extends React.Component {}

    interface ElementProps {
        href?: string;
        isActive?: boolean;
        isChecked?: boolean;
        isDisabled?: boolean;
        isFocused?: boolean;
        isHidden?: boolean;
        isPrimary?: boolean;
        isSelected?: boolean;
        title?: string | null;
        type?: 'link' | 'radio' | 'checkbox' | 'option';
    }

    interface ItemProps {
        appearance?: 'default' | 'primary';
        description?: string;
        elemBefore?: JSX.Element;
        elemAfter?: JSX.Element;
        href?: string | null;
        isActive?: boolean;
        isChecked?: boolean;
        isDisabled?: boolean;
        isFocused?: boolean;
        isHidden?: boolean;
        isPrimary?: boolean;
        isSelected?: boolean;
        tooltipDescription?: string | null;
        tooltipPosition?: "top" | "bottom" | "left" | "right";
        onActivate?(): void;
    }

    export class Item extends React.Component<ItemProps> {}
}

interface IconPropTypes {
    label: string;
    onClick?(): void;
    size?: "small" | "medium" | "large" | "xlarge";
}

declare module '@atlaskit/icon/glyph/chevron-down' {
    export default class CevronDownIcon extends React.Component<IconPropTypes> {}
}

declare module '@atlaskit/icon/glyph/add' {
    export default class AddIcon extends React.Component<IconPropTypes> {}
}

declare module '@atlaskit/icon/glyph/cross-circle' {
    export default class CrossCircleIcon extends React.Component<IconPropTypes> {}
}

declare module '@atlaskit/icon/glyph/calendar' {
    export default class CalendarIcon extends React.Component<IconPropTypes> {}
}

declare module '@atlaskit/field-text' {
    interface TextFieldProps {
        autoComplete?: 'on' | 'off';
        form?: string;
        pattern?: string;
        compact?: boolean;
        type?: string;
        disabled?: boolean;
        isReadOnly?: boolean;
        required?: boolean;
        isInvalid?: boolean;
        invalidMessage?: JSX.Element;
        label?: string;
        name?: string;
        min?: number;
        max?: number;
        placeholder?: string;
        value?: 'string' | 'number';
        onBlur?(): void;
        onChange?(): void;
        onFocus?(): void;
        onKeyDown?(): void;
        onKeyPress?(): void;
        onKeyUp?(): void;
        id?: string;
        isLabelHidden?: boolean;
        shouldFitContainer?: boolean;
        isSpellCheckEnabled?: boolean;
        autoFocus?: boolean;
        maxLength?: number;
    }

    export default class TextField extends React.Component<TextFieldProps> {}
}

declare module '@atlaskit/select' {
    type MenuPlacement = 'auto' | 'bottom' | 'top';
    type Option = {
        label: string;
        value: string;
    }

    // Copied from https://github.com/JedWatson/react-select/blob/v2/src/Select.js#L55
    interface SelectProps {
        /* HTML ID(s) of element(s) that should be used to describe this input (for assistive tech) */
        'aria-describedby'?: string,
        /* Aria label (for assistive tech) */
        'aria-label'?: string,
        /* HTML ID of an element that should be used as the label (for assistive tech) */
        'aria-labelledby'?: string,
        /* Focus the control when it is mounted */
        autoFocus?: boolean,
        /* Remove the currently focused option when the user presses backspace */
        backspaceRemovesValue?: boolean,
        /* Remove focus from the input when the user selects an option (handy for dismissing the keyboard on touch devices) */
        blurInputOnSelect?: boolean,
        /* When the user reaches the top/bottom of the menu, prevent scroll on the scroll-parent  */
        captureMenuScroll?: boolean,
        /* Close the select menu when the user selects an option */
        closeMenuOnSelect?: boolean,
        /* Delimiter used to join multiple values into a single HTML Input value */
        delimiter?: string,
        /* Clear all values when the user presses escape AND the menu is closed */
        escapeClearsValue?: boolean,
        /* Hide the selected option from the menu */
        hideSelectedOptions?: boolean,
        /* The value of the search input */
        inputValue?: string,
        /* Define an id prefix for the select components e.g. {your-id}-value */
        instanceId?: number | string,
        /* Is the select value clearable */
        isClearable?: boolean,
        /* Is the select disabled */
        isDisabled?: boolean,
        /* Is the select in a state of loading (async) */
        isLoading?: boolean,
        /* Support multiple selected options */
        isMulti?: boolean,
        /* Is the select direction right-to-left */
        isRtl?: boolean,
        /* Whether to enable search functionality */
        isSearchable?: boolean,
        /* Async: Text to display when loading options */
        loadingMessage?: (message: { inputValue: string }) => string,
        /* Minimum height of the menu before flipping */
        minMenuHeight?: number,
        /* Maximum height of the menu before scrolling */
        maxMenuHeight?: number,
        /* Maximum height of the value container before scrolling */
        maxValueHeight?: number,
        /* Whether the menu is open */
        menuIsOpen?: boolean,
        /*
        Default placement of the menu in relation to the control. 'auto' will flip
        when there isn't enough space below the control.
        */
        menuPlacement?: MenuPlacement,
        /* Name of the HTML Input (optional - without this, no input will be rendered) */
        name?: string,
        /* Text to display when there are no options */
        noOptionsMessage?: (input: { inputValue: string }) => string,
        /* Handle blur events on the control */
        onBlur?(): void,
        /* Handle change events on the select */
        onChange?(): void,
        /* Handle focus events on the control */
        onFocus?(): void,
        /* Handle change events on the input */
        onInputChange?(): void,
        /* Handle key down events on the select */
        onKeyDown?(): void,
        /* Handle the menu opening */
        onMenuOpen?: () => void,
        /* Handle the menu closing */
        onMenuClose?: () => void,
        /* Array of options that populate the select menu */
        options?: Option[],
        /* Number of options to jump in menu when page{up|down} keys are used */
        pageSize?: number,
        /* Placeholder text for the select value */
        placeholder?: string,
        /* Status to relay to screen readers */
        screenReaderStatus?: (status: { count: number }) => string,
        /* Whether the menu should be scrolled into view when it opens */
        scrollMenuIntoView?: boolean,
        /* Select the currently focused option when the user presses tab */
        tabSelectsValue?: boolean
    }

    export default class Select extends React.Component<SelectProps> {}
}

declare module '@atlaskit/datetime-picker' {
    interface DateTimePickerProps {
        // Whether or not to auto-focus the field.
        autoFocus?: boolean;
        
        // Default for focused.
        defaultFocused?: string;
        
        // Default for isOpen.
        defaultIsOpen?: boolean;
        
        // Default for times.
        defaultTimes?: Array<string>;
        
        // Default for value.
        defaultValue?: string | string[];
        
        // An array of ISO dates that should be disabled on the calendar.
        disabled?: Array<string>;
        
        // Whether or not the field is disabled.
        isDisabled?: boolean;
        
        // Whether or not the dropdown is open.
        isOpen?: boolean;
        
        // The time in the dropdown that should be focused.
        focused?: string;
        
        // Called when the value changes. The first argument is an ISO date and the second is an ISO time.
        onChange: (date: string, time: string) => void;
        
        // The times to show in the dropdown.
        times?: Array<string>;
        
        // The ISO time that should be used as the input value.
        value?: string;
        
        // The width of the field.
        width?: number;
    }

    export class DateTimePicker extends React.Component<DateTimePickerProps> {}
}

declare module '@atlaskit/field-base' {
    interface LabelProps {
        label: string;
    }

    export class Label extends React.Component<LabelProps> {}
}

declare module '@atlaskit/field-text-area' {
    interface FieldTextAreaProps {
        compact?: boolean;
        disabled?: boolean;
        isReadOnly?: boolean;
        required?: boolean;
        isInvalid?: boolean;
        label?: string;
        name?: string;
        placeholder?: string;
        value?: 'string' | 'number';
        onChange?: () => void;
        id?: string;
        isLabelHidden?: boolean;
        invalidMessage?: JSX.Element;
        shouldFitContainer?: boolean;
        isSpellCheckEnabled?: boolean;
        autoFocus?: boolean;
        maxLength?: number;
        minimumRows?: number;
        enableResize?: boolean;
    }
    
    export default class FieldTextArea extends React.Component<FieldTextAreaProps> {}
}

declare module '@atlaskit/spinner' {
    interface SpinnerProps {
        delay?: number;
        invertColor?: boolean;
        onComplete?(): void;
        size?: "small" | "medium" | "large" | "xlarge" | number;
        isCompleting?: boolean;
    }

    export default class Spinner extends React.Component<SpinnerProps> {}
}

declare module '@atlaskit/dynamic-table' {
    export interface DynamicTableHeadCell {
        key: string | number;
        content: JSX.Element;
        isSortable?: boolean;
        width?: number;
        shouldTruncate?: boolean;
    }

    export interface DynamicTableHead {
        cells: DynamicTableHeadCell[];
    }

    export interface DynamicTableCell {
        key: string | number;
        content: JSX.Element;
    }

    export interface DynamicTableRow {
        cells: DynamicTableCell[];
        key: string;
    }    

    export interface RankStart {
        index: number;
        key: string;
    }

    export interface DynamicTableProps {
        defaultPage?: number;
        defaultSortKey?: string;
        defaultSortOrder?: 'ASC' | 'DESC';
        caption?: JSX.Element;
        head?: DynamicTableHead;
        rows?: DynamicTableRow[];
        emptyView?: JSX.Element;
        loadingSpinnerSize?: 'small' | 'large';
        isLoading?: boolean;
        isFixedSize?: boolean;
        rowsPerPage?: number;
        onSetPage?(): void;
        onSort?(): void;
        page?: number;
        sortKey?: string;
        sortOrder?: 'ASC' | 'DESC';
        isRankable?: boolean;
        isRankingDisabled?: boolean;
        onRankStart?: (s: RankStart) => void; 
    }
    
    export default class DynamicTable extends React.Component<DynamicTableProps> {}
}

declare module '@atlaskit/empty-state' {
    interface EmptyStateProps {
        header: string;
        description?: string;
        size?: 'wide' | 'narrow';
        imageUrl?: string;
        maxImageWidth?: number;
        maxImageHeight?: number;
        primaryAction?: JSX.Element;
        secondaryAction?: JSX.Element;
        tertiaryAction?: JSX.Element;
        isLoading?: boolean;
    }

    export default class EmptyState extends React.Component<EmptyStateProps> {}
}