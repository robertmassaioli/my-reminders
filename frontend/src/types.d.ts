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
        onChange?: (element: React.SyntheticEvent<HTMLTextAreaElement>) => void;
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

type OneOrMoreElements = JSX.Element | JSX.Element[];

declare module '@atlaskit/navigation' {
    interface ResizeObj {
        width: number;
        isOpen: boolean;
    }

    type IconAppearance = 'square' | 'round';

    interface NavigationProps {
        /** Elements to be displayed in the ContainerNavigationComponent */
        children?: OneOrMoreElements,
        /** Theme object to be used to color the navigation container. */
        //containerTheme?: Provided,
        /** Component(s) to be rendered as the header of the container.  */
        containerHeaderComponent?: () => OneOrMoreElements,
        /** Standard React ref for the container navigation scrollable element. */
        containerScrollRef?: (ref: React.Ref<any>) => void,
        /** Location to pass in an array of drawers (AkCreateDrawer, AkSearchDrawer, AkCustomDrawer)
         to be rendered. There is no decoration done to the components passed in here. */
        drawers: JSX.Element[],
        /** Theme object to be used to color the global container. */
        //globalTheme?: Provided,
        /** Icon to be used as the 'create' icon. onCreateDrawerOpen is called when it
         is clicked. */
        globalCreateIcon?: JSX.Element,
        /** Icon to be displayed at the top of the GlobalNavigation. This is wrapped in
         the linkComponent. */
        globalPrimaryIcon?: JSX.Element,
        /** Appearance of globalPrimaryIcon for shape styling of drop shadows */
        globalPrimaryIconAppearance: IconAppearance,
        /** Link to be passed to the linkComponent that wraps the globalCreateIcon. */
        globalPrimaryItemHref?: string,
        /** Icon to be used as the 'create' icon. onSearchDrawerOpen is called when it
         is clicked. */
        globalSearchIcon?: JSX.Element,
        /** A list of nodes to be rendered as the global primary actions. They appear
         directly underneath the global primary icon. This must not exceed three nodes */
        globalPrimaryActions?: JSX.Element[],
        /** An array of elements to be displayed at the bottom of the global component.
         These should be icons or other small elements. There must be no more than five.
        Secondary Actions will not be visible when nav is collapsed. */
        globalSecondaryActions?: JSX.Element[],
        /** Whether to display a scroll hint shadow at the top of the ContainerNavigation
         * wrapper. */
        hasScrollHintTop?: boolean,
        /** Set whether collapse should be allowed. If false, the nav cannot be dragged
         to be smaller. */
        isCollapsible?: boolean,
        /** Set whether the nav is collapsed or not. Note that this is never controlled
         internally as state, so if it is collapsible, you need to manually listen to onResize
        to determine when to change this if you are letting users manually collapse the
        nav. */
        isOpen?: boolean,
        /** Sets whether to disable all resize prompts. */
        isResizeable?: boolean,
        /** Causes leftmost navigation section to be slightly wider to accommodate macOS buttons. */
        isElectronMac?: boolean,
        /** A component to be used as a link. By Default this is an anchor. when a href
         is passed to it, and otherwise is a button. */
        linkComponent?: React.ComponentType<any>,
        /** Function called at the end of a resize event. It is called with an object
         containing a width and an isOpen. These can be used to update the props of Navigation. */
        onResize?: (resizeState: ResizeObj) => void,
        /** Function to be called when a resize event starts. */
        onResizeStart?: () => void,
        /** Function called when the globalCreateIcon is clicked. */
        onCreateDrawerOpen?: () => void,
        /** Function called when the globalSearchIcon is clicked. */
        onSearchDrawerOpen?: () => void,
        /** Function called when a collapse/expand starts */
        onToggleStart?: () => void,
        /** Function called when a collapse/expand finishes */
        onToggleEnd?: () => void,
        /** The offset at the top of the page before the navigation begins. This allows
         absolute items such as a banner to be placed above nav, without lower nav items
        being pushed off the screen. **DO NOT** use this outside of this use-case. Changes
        are animated. The string is any valid css height value */
        topOffset?: number,
        /** Width of the navigation. Width cannot be reduced below the minimum, and the
         collapsed with will be respected above the provided width. */
        width?: number,
    }

    export default class Navigation extends React.Component<NavigationProps> {}

    interface AkGlobalItemProps {
        /** Standard aria-haspopup prop */
        'aria-haspopup'?: string, // eslint-disable-line react/no-unused-prop-types
        /** Element to be rendered inside the item. Should be an atlaskit icon. */
        children?: OneOrMoreElements,
        /** href to pass to linkComponent.  */
        href?: string,
        /** Causes the item to appear with a persistent selected background state. */
        isSelected?: boolean,
        /** Component to be used to create the link in the global item. A default
         component is used if none is provided. */
        linkComponent?: React.ComponentType<any>,
        /** Standard onClick event */
        onClick?: (event: Event, data?: {}) => void,
        onMouseDown?: (event: MouseEvent) => void,
        /** ARIA role to apply to the global item. */
        role?: string,
        /** Set the size of the item's content.  */
        size?: 'small' | 'medium' | 'large',
        /** Appearance of item for custom styling (square or round) */
        appearance?: IconAppearance,
    }

    export class AkGlobalItem extends React.Component<AkGlobalItemProps> {}

    export class AkNavigationItemGroup extends React.Component {}

    interface AkNavigationItemProps {
        action?: JSX.Element,
        /** Text to appear to the right of the text. It has a lower font-weight. */
        caption?: string,
        /** Drag and drop props provided by react-beautiful-dnd. Please do not use
         * this unless using react-beautiful-dnd */
        //dnd?: DnDType,
        /** Location to link out to on click. This is passed down to the custom link
         component if one is provided. */
        href?: string,
        /** Target frame for item `href` link to be aimed at. */
        target?: string,
        /** React element to appear to the left of the text. This should be an
         @atlaskit/icon component. */
        icon?: Node,
        /** Element displayed to the right of the item. The dropIcon should generally be
         an appropriate @atlaskit icon, such as the ExpandIcon. */
        dropIcon?: Node,
        /** Makes the navigation item appear with reduced padding and font size. */
        isCompact?: boolean,
        /** Used to apply correct dragging styles when also using react-beautiful-dnd. */
        isDragging?: boolean,
        /** Set whether the item should be highlighted as selected. Selected items have
         a different background color. */
        isSelected?: boolean,
        /** Set whether the item should be used to trigger a dropdown. If this is true,
         The href property will be disabled. */
        isDropdownTrigger?: boolean,
        /** Component to be used as link, if default link component does not suit, such
         as if you are using a different router. Component is passed a href prop, and the content
        of the title as children. Any custom link component must accept a className prop so that
        it can be styled. */
        linkComponent?: React.ComponentType<any>,
        /** Function to be called on click. This is passed down to a custom link component,
         if one is provided.  */
        onClick?: (event?: MouseEvent) => void,
        /** Function to be called on click. This is passed down to a custom link component,
         if one is provided.  */
        onKeyDown?: (e: KeyboardEvent) => void,
        /** Standard onmouseenter event */
        onMouseEnter?: (e: MouseEvent) => void,
        /** Standard onmouseleave event */
        onMouseLeave?: (e: MouseEvent) => void,
        /** Text to be shown alongside the main `text`. */
        subText?: string,
        /** Main text to be displayed as the item. Accepts a react component but in most
         cases this should just be a string. */
        text?: JSX.Element | string,
        /** React component to be placed to the right of the main text. */
        textAfter?: Node,
        /** Whether the Item should attempt to gain browser focus when mounted */
        autoFocus?: boolean,
    }

    export class AkNavigationItem extends React.Component<AkNavigationItemProps> {}

    interface AkContainerTitleProps {
        /** Location to link out to on click. This is passed down to the custom link
         component if one is provided. */
        href?: string,
        /** React element to appear to the left of the text. This should be an
         @atlaskit/icon component. */
        icon?: JSX.Element,
        /** Component to be used as link, if default link component does not suit, such
         as if you are using a different router. Component is passed a href prop, and the content
        of the title as children. Any custom link component must accept a className prop so that
        it can be styled. */
        linkComponent?: React.ComponentType<any>,
        /** Function to be called on click. This is passed down to a custom link component,
         if one is provided.  */
        onClick?: (event?: MouseEvent) => void,
        /** Function to be called on click. This is passed down to a custom link component,
         if one is provided.  */
        onKeyDown?: (e: KeyboardEvent) => void,
        /** Standard onmouseenter event */
        onMouseEnter?: (e: MouseEvent) => void,
        /** Standard onmouseleave event */
        onMouseLeave?: (e: MouseEvent) => void,
        /** Text to be shown alongside the main `text`. */
        subText?: string,
        /** Main text to be displayed as the item. Accepts a react component but in most
         cases this should just be a string. */
        text?: JSX.Element | string,
    }

    export class AkContainerTitle extends React.Component<AkContainerTitleProps> {}
}

declare module '@atlaskit/tooltip' {
    type PositionType = 'bottom' | 'left' | 'right' | 'top';

    interface TooltipProps {
        /** A single element, either Component or DOM node */
        children: JSX.Element[] | JSX.Element,
        /** The content of the tooltip */
        content: JSX.Element | string,
        /** Extend `TooltipPrimitive` to create you own tooptip and pass it as component */
        component?: React.ComponentType<{ innerRef: (element: HTMLElement) => void }>,
        /** Hide the tooltip when the element is clicked */
        hideTooltipOnClick?: boolean,
        /** Function to be called when a mouse leaves the target */
        onMouseOut?: (event: MouseEvent) => void,
        /** Function to be called when a mouse enters the target */
        onMouseOver?: (event: MouseEvent) => void,
        /** Where the tooltip should appear relative to its target */
        position?: PositionType,
        /** Replace the wrapping element */
        tag?: string,
        /** Show only one line of text, and truncate when too long */
        truncate?: boolean,
    }

    export default class Tooltip extends React.Component<TooltipProps> {}
}

declare module '@atlaskit/page' {
    interface PageProps {
        banner?: JSX.Element,
        children?: JSX.Element[] | JSX.Element,
        isBannerOpen?: JSX.Element,
        navigation?: JSX.Element,
    }

    export default class Page extends React.Component<PageProps> {}
}

declare namespace AP {
    function resize(): void;

    interface StandardRequestOptions {
        url: string;
        cache?: false;
        headers?: { [header: string]: string };
        // headers TODO
    }

    interface GetRequestOptions extends StandardRequestOptions {
        type: 'GET'
    }

    interface PostPutRequestOptions extends StandardRequestOptions {
        type: 'POST' | 'PUT';
        data: any;
        contentType: string;
    }

    interface DeleteRequestOptions extends StandardRequestOptions {
        type: 'DELETE';
        data?: any;
        contentType?: string;
    }

    interface RequestResponse {
        body: string;
        xhr: XMLHttpRequest;
    }

    function request(options: GetRequestOptions | PostPutRequestOptions | DeleteRequestOptions): Promise<RequestResponse>;

    namespace flag {
        interface Flag {

        }

        interface FlagCreateOptions {
            title: string;
            body: string;
            type?: "info" | "success" | "warning" | "error";
            close?: 'manual' | 'auto';
        }

        function create(options: FlagCreateOptions): Flag;
    }

    namespace dialog {
        interface DialogCommonOptions {
            key: string;
            chrome?: boolean;
            header?: string;
            submitText?: string;
            cancelText?: string;
            hint?: string;
            customData?: object;
            closeOnEscape?: boolean;
        }

        interface DialogSizeOptions extends DialogCommonOptions {
            size: 'small' | 'medium' | 'large' | 'x-large' | 'fullscreen';
        }

        interface DialogDimensionOptions extends DialogCommonOptions {
            width: string;
            height: string;
        }
          
        interface Dialog {
            on: (event: 'close', callbackFn: () => void) => void;
        }

        function create(options: DialogSizeOptions | DialogDimensionOptions) : Dialog;

        function close(data?: object): void;
    }

    namespace events {
        type EventData = object;

        type EventListner = (data: EventData) => void;
        type EventFilter = (data: EventData) => boolean;

        function on(name: string, listener: EventListener): void;

        function onPublic(name: string, listener: EventListener, filter: EventFilter): void;

        function once(name: string, listener: EventListener): void;

        function oncePublic(name: string, listener: EventListener, filter: EventFilter): void;

        function onAny(listener: EventListener): void;

        function onAnyPublic(listener: EventListener, filter: EventFilter): void;

        function off(name: string, listener: EventListener): void;

        function offPublic(name: string, listener: EventListener): void;

        function offAll(name: string): void;

        function offAllPublic(name: string): void;

        function offAny(listener: EventListener): void;

        function offAnyPublic(listener: EventListener): void;

        function emit(name: string, ...args: EventData[]): void;

        function emitPublic(name: string, ...args: EventData[]): void;
    }
}