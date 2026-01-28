import React, { useState } from "react";
import ForgeReconciler, {
  Text,
  Heading,
  Button,
  Tooltip,
  Label,
  Link,
  Modal,
  ModalBody,
  ModalHeader,
  ModalTitle,
  SectionMessage,
  DatePicker,
  TimePicker,
  Select,
  TextArea,
  Form,
  FormFooter,
  HelperMessage,
  RequiredAsterisk,
  ModalFooter,
  useForm,
  Stack,
  Inline,
  xcss,
  Box,
  Strong,
} from "@forge/react";
import { invoke, requestJira, view } from "@forge/bridge";
import { useEffectAsync } from "../useEffectAsync";
import moment from 'moment-timezone/builds/moment-timezone-with-data';
import { isPresent } from "ts-is-present";
import { toDateOutput } from "./dateHelpers";

function generateTimeOptions(startHour, endHour) {
  const times = [];
  for (let hour = startHour; hour <= endHour; hour++) {
    const hourStr = hour.toString().padStart(2, '0');
    times.push(`${hourStr}:00`);
    if (hour < endHour) { // Don't add :30 for the last hour
      times.push(`${hourStr}:30`);
    }
  }
  return times;
}

function getRandomTimeInHour(baseHour) {
  const randomMinute = Math.floor(Math.random() * 60);
  return { hour: baseHour, minute: randomMinute };
}

// Individual Time Calculator Functions
function getTomorrowMorning() {
  const randomTime = getRandomTimeInHour(6);
  return moment()
    .add(1, 'day')
    .hour(randomTime.hour)
    .minute(randomTime.minute)
    .second(0);
}

function getIn24Hours() {
  return moment().add(24, 'hours').second(0);
}

function getNextMonday() {
  const randomTime = getRandomTimeInHour(6);
  return moment()
    .day(1) // Monday
    .add(1, 'week')
    .hour(randomTime.hour)
    .minute(randomTime.minute)
    .second(0);
}

function getInSevenDays() {
  return moment().add(7, 'days').second(0);
}

function getInOneMonth() {
  const randomTime = getRandomTimeInHour(6);
  return moment()
    .add(1, 'month')
    .hour(randomTime.hour)
    .minute(randomTime.minute)
    .second(0);
}

function getNextQuarter() {
  const randomTime = getRandomTimeInHour(6);
  return moment()
    .add(3, 'months')
    .hour(randomTime.hour)
    .minute(randomTime.minute)
    .second(0);
}

function getInOneYear() {
  const randomTime = getRandomTimeInHour(6);
  return moment()
    .add(1, 'year')
    .hour(randomTime.hour)
    .minute(randomTime.minute)
    .second(0);
}

const buttonGroupBoxStyle = xcss({
  marginTop: "space.100",
});

const cardStyle = xcss({
  padding: "space.100",
  borderBottomColor: "color.border",
  borderBottomWidth: "border.width",
  borderBottomStyle: "solid",
});

const remindersSectionStyle = xcss({
  marginTop: "space.100",
});

const remindersInnerStyle = xcss({
  marginTop: "space.100",
  borderTopColor: "color.border",
  borderTopWidth: "border.width",
  borderTopStyle: "solid",
});

const sectionMessageStyle = xcss({
  marginTop: "space.100",
});

const reminderTitleStyle = xcss({
  marginTop: "space.100",
});


async function createReminder({ issueSummary, timestamp, message }) {
  const response = await invoke("createReminder", {
    issueSummary,
    timestamp,
    message,
  });

  return response;
}

async function getIssueSummary(context) {
  const response = await requestJira(
    `/rest/api/3/issue/${context.extension.issue.id}`,
    {
      headers: {
        "Content-type": "application/json",
        Accept: "application/json",
      },
    }
  );
  const issueData = await response.json();
  return issueData.fields.summary;
}

const App = () => {
  const [isAddReminderOpen, setAddReminderOpen] = useState(false);
  const [quickSelectValue, setQuickSelectValue] = useState("");

  // Generic Quick Reminder Creation Function
  async function createQuickReminder(timeCalculator, logLabel) {
    console.log(`${logLabel} - Starting`);
    const context = await view.getContext();
    const issueSummary = await getIssueSummary(context);

    const targetTime = timeCalculator();
    console.log(`${logLabel} - Reminder time:`, targetTime.format());

    const response = await createReminder({
      issueSummary,
      timestamp: targetTime.unix(),
      message: undefined,
    });

    console.log(`${logLabel} - Response:`, response);
    
    if (isPresent(response.errors)) {
      console.error(`${logLabel} - Errors:`, response.errors);
    } else {
      console.log(`${logLabel} - Success, updating reminders`);
      setReminders(response.reminders);
    }
  }
  const [reminders, setReminders] = useState(undefined);
  const [userTimezone, setUserTimezone] = useState(undefined);
  const [expiredRemindersWebtrigger, setExpiredRemindersWebtrigger] =
    useState(undefined);
  const { handleSubmit, register } = useForm();
  useEffectAsync(async () => {
    setExpiredRemindersWebtrigger(await invoke("getExpirySchedulerWebTrigger"));
  }, []);

  useEffectAsync(async () => {
    const data = await invoke("getMyReminders");
    setReminders(data.reminders);
  }, []);

  useEffectAsync(async () => {
    const context = await view.getContext();
    setUserTimezone(context.timezone);
  }, []);

  if (isPresent(userTimezone)) {
    moment.tz.setDefault(userTimezone);
  }


  // Handle quick select changes
  async function handleQuickSelectChange(option) {
    console.log("Quick select triggered:", option);
    
    if (!option?.value) return;
    
    const selectedValue = option.value;
    setQuickSelectValue(""); // Reset select immediately
    
    const actions = {
      'tomorrow-morning': () => createQuickReminder(getTomorrowMorning, 'Tomorrow Morning'),
      'in-24-hours': () => createQuickReminder(getIn24Hours, 'In 24 Hours'),
      'next-monday': () => createQuickReminder(getNextMonday, 'Next Monday'),
      'in-seven-days': () => createQuickReminder(getInSevenDays, 'In Seven Days'),
      'in-month': () => createQuickReminder(getInOneMonth, 'In a Month'),
      'next-quarter': () => createQuickReminder(getNextQuarter, 'Next Quarter'),
      'in-year': () => createQuickReminder(getInOneYear, 'In a Year'),
    };
    
    const action = actions[selectedValue];
    if (action) {
      await action();
    } else {
      console.log("Unknown option:", selectedValue);
    }
  }

  async function createArbitraryReminder({ expiryDate, expiryTime, message }) {
    const context = await view.getContext();
    const issueSummary = await getIssueSummary(context);

    // Parse the time string (format: "HH:MM") and set it on the date
    const [hours, minutes] = expiryTime.split(':').map(Number);
    const expiry = moment(expiryDate)
      .hour(hours)
      .minute(minutes)
      .second(0); // Keep seconds at 0 for clean timestamps

    const response = await createReminder({
      issueSummary,
      timestamp: expiry.unix(),
      message,
    });

    if (isPresent(response.errors)) {
      // TODO How do I flash the errors?
    } else {
      setReminders(response.reminders);
    }
  }

  async function deleteReminder(reminderKey) {
    const data = await invoke("deleteReminder", { reminderKey });

    setReminders(data.reminders);
  }


  const create = async (data) => {
    setAddReminderOpen(false);
    createArbitraryReminder(data);
  };

  return (
    <>
      <Heading as="h3">Add reminder</Heading>
      <Box xcss={buttonGroupBoxStyle}>
        <Inline shouldWrap space="space.100">
          <Button onClick={() => setAddReminderOpen(true)}>
            Select a time...
          </Button>
          <Select
            placeholder="Quick options..."
            value={quickSelectValue}
            onChange={handleQuickSelectChange}
            options={[
              { label: "Tomorrow Morning", value: "tomorrow-morning" },
              { label: "In 24 hours", value: "in-24-hours" },
              { label: "Next Monday", value: "next-monday" },
              { label: "In seven days", value: "in-seven-days" },
              { label: "In a month", value: "in-month" },
              { label: "Next quarter", value: "next-quarter" },
              { label: "In a year", value: "in-year" }
            ]}
          />
        </Inline>
      </Box>
      {reminders && reminders.length > 0 && (
        <Box xcss={remindersSectionStyle}>
          <Heading as="h3">Your reminders</Heading>
          <Box xcss={remindersInnerStyle}>
            <Stack grow="fill">
              {reminders.map((reminder) => {
                const { date, message } = reminder.value;
                const expiry = moment.unix(date);
                const fullDateOutput = toDateOutput(expiry);
                return (
                  <Box xcss={cardStyle}>
                    <Stack>
                      <Inline spread="space-between">
                        <Box xcss={reminderTitleStyle}>
                          <Tooltip content={fullDateOutput}>
                            <Strong>{expiry.fromNow()}</Strong>
                          </Tooltip>
                        </Box>
                        <Button
                          iconBefore="editor-remove"
                          onClick={() => deleteReminder(reminder.key)}
                        />
                      </Inline>
                      {message && <Text>{message || "<No Message>"}</Text>}
                    </Stack>
                  </Box>
                );
              })}
            </Stack>
          </Box>
        </Box>
      )}
      {reminders && reminders.length === 0 && (
        <Box xcss={sectionMessageStyle}>
          <SectionMessage>
            <Text>You have no reminders on this issue. Create some!</Text>
          </SectionMessage>
        </Box>
      )}
      {reminders === undefined && (
        <Box xcss={sectionMessageStyle}>
          <SectionMessage>
            <Text>Loading your reminders...</Text>
          </SectionMessage>
        </Box>
      )}
      <Text>
        Your timezone:{" "}
        <Link href="/secure/ViewPersonalSettings.jspa">{userTimezone}</Link>
      </Text>
      {isAddReminderOpen && (
        <Modal onClose={() => setAddReminderOpen(false)}>
          <ModalHeader>
            <ModalTitle>Add a reminder</ModalTitle>
          </ModalHeader>
          <ModalBody>
            <Form onSubmit={handleSubmit(create)}>
              <Label labelFor="expiryDate">Expiry date</Label>
              <RequiredAsterisk />
              <DatePicker
                defaultValue={moment().add(1, "day").format("YYYY-MM-DD")}
                name="expiryDate"
                id="expiryDate"
                autoFocus={false}
                defaultIsOpen={false}
                dateFormat="YYYY/MM/DD"
                weekStartDay={1}
                {...register("expiryDate", { required: true })}
              />
              <HelperMessage>
                Set this date to the day that you want your reminder to be sent
              </HelperMessage>
              <Label labelFor="expiryTime">Time</Label>
              <RequiredAsterisk />
              <TimePicker
                name="expiryTime"
                id="expiryTime"
                defaultValue="09:00"
                isRequired={true}
                timeIsEditable={true}
                autoFocus={false}
                defaultIsOpen={false}
                times={generateTimeOptions(6, 20)}
                {...register("expiryTime", { required: true })}
              />
              <HelperMessage>
                What time do you want your reminder to be sent?
              </HelperMessage>
              <Label labelFor="message">Reminder message</Label>
              <TextArea
                name="message"
                placeholder="Optional message to go with your reminder"
                {...register("message")}
              />
              <FormFooter>
                <Button appearance="primary" type="submit">
                  Save
                </Button>
              </FormFooter>
            </Form>
          </ModalBody>
          <ModalFooter />
        </Modal>
      )}
      {/* <Text>Reminder Data: {JSON.stringify(reminders, null, 2)}</Text> */}
      {/* <Text>Check for expired Reminders: {expiredRemindersWebtrigger}</Text> */}
      {/* <Text>Issue Summary: {issueSummary}</Text> */}
    </>
  );
};

ForgeReconciler.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>
);
