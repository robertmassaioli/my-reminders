<apply template="connect-panel">
    <bind tag="body-class">create-reminder</bind>
    <bind tag="header-extra">
        <js src="/static/${resourcesVersion}/js/app/create-reminder-v2.js" />
        <script>require(['app/create-reminder-v2']);</script>

        <css href="/static/${resourcesVersion}/css/app.css" />
    </bind>

    <section role="dialog" id="add-reminder-dialog" class="ac-dialog aui-layer aui-dialog2 aui-dialog2-medium">
        <!-- Dialog header -->
        <header class="aui-dialog2-header">
            <h2 class="aui-dialog2-header-main">Create a reminder</h2>
        </header>
        <!-- Main dialog content -->
        <div class="aui-dialog2-content">
            <form action="#" method="post" id="d" class="aui">
                <fieldset>
                    <div class="field-group">
                        <label for="reminderDate">When<span class="aui-icon icon-required"> required</span></label>
                        <input class="text" type="text" id="reminderDate" name="reminderDate" title="when">
                        <!--div class="description">Default width input of a required field</div-->
                    </div>
                    <div class="field-group">
                        <label for="reminderMessage">Message</label>
                        <textarea class="textarea" rows="4" cols="10" name="reminderMessage" id="reminderMessage" placeholder="Your message here..."></textarea>
                        <div class="description">A short optional reminder message</div>
                    </div>
                </fieldset>
            </form>
        </div>
        <!-- Dialog footer -->
        <footer class="aui-dialog2-footer">
            <!-- Actions to render on the right of the footer -->
            <div class="aui-dialog2-footer-actions">
                <button id="button-add" class="aui-button">Add</button>
                <button id="button-close" class="aui-button aui-button-link">Close</button>
            </div>
            <!-- Hint text is rendered on the left of the footer -->
            <div class="aui-dialog2-footer-hint">Create a reminder for yourself</div>
        </footer>
    </section>
</apply>
