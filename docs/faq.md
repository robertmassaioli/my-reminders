## Frequently Asked Questions

### Getting Started

#### How do I install and configure My Reminders?

My Reminders can be installed from the Atlassian Marketplace by your Jira administrator. Once installed:

1. Navigate to any Jira issue
2. Look for the "My Reminders" section in the issue view
3. Click "Set Reminder" to create your first reminder
4. Choose your reminder date and add an optional custom message

No additional configuration is required - the app works immediately after installation.

#### What permissions does My Reminders need and why?

My Reminders requires the following permissions:

 - **Read access to issues**: To display issue details in reminders
 - **Write access for notifications**: To send reminder notifications through Jira
 - **User profile access**: To identify who should receive reminders

These permissions ensure the app can function while maintaining security within your Jira instance.

#### Is My Reminders available for Jira Server/Data Center or only Jira Cloud?

My Reminders is currently available for **Jira Cloud only**. The app is built using Atlassian's Forge platform and runs entirely within Atlassian's infrastructure, which is specific to Cloud instances.

### Using My Reminders

#### How do I create or delete a reminder?

**Creating a reminder:**
1. Open any Jira issue
2. Find the "My Reminders" section
3. Click "Set Reminder"
4. Select your reminder date and time
5. Add an optional custom message
6. Click "Save"

**Deleting a reminder:**
1. Find the reminder in the issue view
2. Click "Delete" next to the reminder
3. Confirm the deletion

**Note:** Reminders cannot be edited once created. If you need to change a reminder's date, time, or message, you must delete the existing reminder and create a new one.

#### Can I see all my active reminders in one place?

Yes! Navigate to the "Your Reminders" page accessible from:
- The Jira main menu
- The My Reminders dashboard gadget (if added to your dashboard)

This view shows all your active reminders sorted by date, allowing you to manage them in bulk.

#### What happens if I delete or move an issue that has reminders?

- **Deleted issues**: Reminders are automatically cancelled and removed from the system
- **Moved issues**: Reminders remain active and will still be delivered, with links updated to the new issue location
- **Closed issues**: Reminders continue to work normally - you may still want to be reminded about closed issues

#### Can I set recurring reminders?

Currently, My Reminders supports one-time reminders only. Each reminder fires once at the specified date and time. If you need recurring reminders, you'll need to create multiple individual reminders.

### About Our Features

#### Can I set a reminder for another person?

No, by design. You can only set reminders for yourself. This prevents potential abuse of the reminder system and unwanted notifications to colleagues.

If you want to ensure someone else doesn't forget an issue, set a reminder for yourself with a message like "Follow up with person] about this issue."

#### How far in the future can I set reminders?

You can set reminders for any future date. The app provides quick options up to one year (through the "In a year" button), but you can use the custom date picker to set reminders as far into the future as needed.

#### What time zones are supported for reminders?

Reminders use your Jira user profile's timezone setting. The app automatically converts reminder times to your local timezone for both setting and delivery.

#### What notification formats are available?

Reminders are delivered as **Jira notifications**, which means:
- Jira will send an email to your registered email address
- The notification appears in your Jira notification center
- Email format follows your Jira notification preferences
- No external email service is used - everything stays within Atlassian's platform

### Data and Security

#### What data do you store about my issues?

We store only the minimal data required for reminder functionality:

 - Issue Key / ID (to direct you to the right issue)
 - Issue Summary (to identify the issue in notifications)
 - Your username (to know who owns the reminder)
 - Your custom message (for personalized reminder content)
 - Reminder date and time (to know when to send the reminder)

All data remains within Atlassian's infrastructure and never leaves the platform.

#### How long do you store my data for?

Reminder data is stored until the reminder is delivered, then automatically deleted within 30 days. If you delete a reminder before it fires, the data is removed immediately.

#### Does My Reminders use any external services?

No. My Reminders achieves the "Runs on Atlassian" badge, meaning:
- All processing occurs within Atlassian's infrastructure
- No data is sent to external services
- All functionality is provided by Atlassian's platform capabilities
- Maximum security and compliance standards are maintained

### Troubleshooting

#### How do I troubleshoot if reminders aren't working?

If you're not receiving reminders:

1. **Check your email settings**: Ensure Jira notifications are enabled in your profile
2. **Verify reminder date**: Confirm the reminder date has passed
3. **Check spam folder**: Jira notifications might be filtered by your email provider
4. **Confirm issue access**: Ensure you still have permission to view the issue
5. **Download app logs**: Administrators can go to the Connected Apps page, find "My Reminders", and click "Download logs" to see detailed information about why reminders did or did not send
6. **Contact your administrator**: They can check app installation and permissions using the log information above

#### How do I report bugs or suggest features?

You can report bugs or suggest new features directly at: https://github.com/robertmassaioli/my-reminders/issues

This GitHub repository is actively monitored by the development team and provides the fastest way to:
- Report bugs with detailed information
- Request new features
- Track the status of your submissions
- Participate in discussions about improvements