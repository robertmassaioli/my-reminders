<bind tag="js">
   <script type="text/javascript" src="${href}"></script>
</bind>

<bind tag="stylesheet">
   <link rel="stylesheet" type="text/css" href="${href}">
</bind>

<apply template="connect-panel">
   <bind tag="header-extra">
      <js><bind tag="href">/static/create-reminder.js</bind></js>
      <stylesheet><bind tag="href">/static/create-reminder.css</bind></stylesheet>

      <js><bind tag="href">//cdnjs.cloudflare.com/ajax/libs/URI.js/1.11.2/URI.min.js</bind></js>
   </bind>

   <!-- TODO use aui experimental here too so that you get the advantage of all features -->
   <!-- TODO work out per user which one gets pressed the most and optimise for that -->
<!-- TODO add AUI validation -->
<!-- TODO textinput instead of textarea because textinputs can have a maxlength -->
<!-- TODO Test showing the form permenantly -->
    
<div id="upcoming-reminders" class="reminders">
    <span>Upcoming reminders:</span>
    <span title="Ping me, when I am needed..." id="closeable-label-nourl" class="reminder aui-label aui-label-closeable">Tomorrow<span tabindex="0" class="aui-icon aui-icon-close" original-title="(remove closableNoUrl)">(remove closableNoUrl)</span></span>
    <span title="Friday, friday, gotta view issues on friday..." id="closeable-label-nourl" class="reminder aui-label aui-label-closeable">In 2 days<span tabindex="0" class="aui-icon aui-icon-close" original-title="(remove closableNoUrl)">(remove closableNoUrl)</span></span>
    <span id="closeable-label-nourl" class="reminder aui-label aui-label-closeable">In 5 days<span tabindex="0" class="aui-icon aui-icon-close" original-title="(remove closableNoUrl)">(remove closableNoUrl)</span></span>
    <span title="Baby come back, I don't wanna loose you." id="closeable-label-nourl" class="reminder aui-label aui-label-closeable">20 Dec 2013<span tabindex="0" class="aui-icon aui-icon-close" original-title="(remove closableNoUrl)">(remove closableNoUrl)</span></span>
    <span title="I am bad, and that is good. I will never be good and that's not bad, because there's no person that I would rather be than me." id="closeable-label-nourl" class="reminder aui-label aui-label-closeable">1 Jan 2014<span tabindex="0" class="aui-icon aui-icon-close" original-title="(remove closableNoUrl)">(remove closableNoUrl)</span></span>
</div>
<!-- TODO maybe aria-pressed="true" -->
<div class="reminder-actions aui-buttons">
   <button id="add-reminder" class="aui-button">Add reminder</button>
   <button id="add-reminder-tomorrow" class="aui-button aui-button-link">Tomorrow</button>
   <button id="add-reminder-next-week" class="aui-button aui-button-link">Next Week</button>
   <span class="status-container">
        <span id="reminder-creation-pending" class="aui-icon aui-icon-wait">Wait</span>
        <span id="reminder-creation-success" class="aui-icon aui-icon-success">Success</span>
        <span id="reminder-creation-error"   class="aui-icon aui-icon-error">Error</span>
    </span>
</div>
<form action="#" method="post" id="create-reminder-form" class="aui hidden">
    <fieldset>
        <div class="field-group custom-container">
            <label for="d-fname">When<span class="aui-icon icon-required"> required</span></label>
            <input id="custom-ping-magnitude" class="text short-field" type="text" title="magnitude" value="1">
            <select id="custom-ping-timeunit" class="select short-field" title="time unit">
                <option value="Day" selected="selected">Day</option>
                <option value="Week">Week</option>
                <option value="Month">Month</option>
                <option value="Year">Year</option>
            </select>
            <div class="description">The time when you should be notified</div>
        </div>
        <div class="field-group custom-container">
            <label for="licenseKey">Message</label>
            <textarea id="custom-ping-message" class="textarea" rows="3" cols="10"></textarea>
            <div class="description">A short optional reminder mesage</div>
        </div>
    </fieldset>
    <div class="buttons-container custom-container custom-operations">
        <div class="buttons">
            <input class="button submit" type="submit" value="Add" id="d-save-btn1">
            <a class="cancel" href="#">Close</a>
        </div>
    </div>
</form>
</apply>
