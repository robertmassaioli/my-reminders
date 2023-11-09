## Frequently Asked Questions

### About our features

These are the most common questions that we have about our features and our answers to those
questions:

#### Can I set a reminder for another person?

The short answer is: no, by design. The intention is that you can only set reminders for yourself. Being able 
to set reminders for other people opens up the potential for abusing the reminders system. 
We wanted to prevent people from spamming their co-workers with reminders. In the spirit of 
fostering good communication we currently do not intend to allow the creation of reminders for 
other people.

If you want to make sure that somebody else does not forget about an issue then you can
set a reminder for yourself that says something like "Remind bob not to forget this issue".

#### How far in the future can I set reminders for in this service?

We will not forget about your reminders and are committed to ensuring that your reminders make it to 
you. However, any reminder that is set for more than three years in the future is not currently
supported. As this service matures we will be looking to extend this period of supported time.

#### How do I tell you about a bug or a great new idea for a feature?

If you have found a bug or you have a great new idea for a feature then you can [raise an issue][1]
for us and we will get to it as soon as possible.

### About the Service

These are the common questions that we get about the running of our service and our answers to those
questions:

### What data do you store about my issues?

We currently store the following data:

 - Issue Key / Id (So that we can direct you to the rigth issue)
 - Issue Summary (So that we can tell you what the issue is about)
 - Your username (So that we know who this reminder belongs to)
 - Your email address (So that we know where to send your reminder)
 - Your custom message (So that we know what to tell you to jog your memory)
 - Date to expire your reminder (So that we know when to send you the reminder)

We have only taken and stored the data that we strictly need to make this a fast and reliable
service for you.

### How long do you store my data for?

In order to remind you about anything we need to store that information until the date you should be
reminded. We aim to delete your data within 30 days of us sending the reminder but may retain it for
longer.

### What external services do you rely upon?

We feel the need to tell you about our external dependencies because, if they were to fail, then we
will fail:

 - Our service runs on [AWS][5].
 - We have scheduled tasks (like sending your reminders) that are triggered by the [EazyCron service][4].

If any of these services should fail then our functionality would be degraded in some manner.
However we are monitoring those services closely and will know immediately if they do fail. Meaning
that if there is a problem that we will be the first to know and be looking to fix it immediately.

 [1]: /redirect/raise-issue
 [4]: http://www.easycron.com/
 [5]: http://aws.amazon.com/
