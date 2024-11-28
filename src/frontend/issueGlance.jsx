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
  Select,
  TextArea,
  Form,
  FormFooter,
  ButtonGroup,
  DynamicTable,
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
import moment from "moment-timezone";
import { isPresent } from "ts-is-present";
import { toDateOutput } from "./dateHelpers";

const buttonGroupBoxStyle = xcss({
  marginTop: 'space.100'
});

const cardStyle = xcss({
  padding: 'space.100',
  marginRight: 'space.100',
  borderBottomColor: 'color.border',
  borderBottomWidth: 'border.width',
  borderBottomStyle: 'solid',

});

const remindersSectionStyle = xcss({
  marginTop: 'space.100'
});

const remindersInnerStyle = xcss({
  borderTopColor: 'color.border',
  borderTopWidth: 'border.width',
  borderTopStyle: 'solid',
});

const reminderTitleStyle = xcss({
  marginTop: 'space.100'
});

function generateHoursOfDay() {
  return Array.from({ length: 24 }, (_, index) => {
    const isAfternoon = index >= 12;
    const lowerMeridien = isAfternoon ? "pm" : "am";
    let rawHour = index >= 12 ? index - 12 : index;
    const displayLowerHour = rawHour === 0 ? 12 : rawHour;
    const displayUpperHour = rawHour + 1;
    const upperIs12 = displayUpperHour === 12;
    const upperMeridien = isAfternoon
      ? upperIs12
        ? "am"
        : "pm"
      : upperIs12
      ? "PM"
      : "AM";
    return {
      index,
      display: `${displayLowerHour}${lowerMeridien}-${displayUpperHour}${upperMeridien}`,
    };
  });
}

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

  async function createReminderForTomorrow() {
    const context = await view.getContext();
    const issueSummary = await getIssueSummary(context);

    const tomorrow = moment()
      .add(1, "day")
      .set("hour", 6 + Math.round(Math.random() * 2));

    const response = await createReminder({
      issueSummary,
      timestamp: tomorrow.unix(),
      message: undefined,
    });

    if (isPresent(response.errors)) {
      // TODO How do I flash the errors?
    } else {
      setReminders(response.reminders);
    }
  }

  async function createReminderForNextWeek() {
    const context = await view.getContext();
    const issueSummary = await getIssueSummary(context);

    const nextWeek = moment()
      .add(7, "day")
      .set("hour", 6 + Math.round(Math.random() * 2));

    const response = await createReminder({
      issueSummary,
      timestamp: nextWeek.unix(),
      message: undefined,
    });

    if (isPresent(response.errors)) {
      // TODO How do I flash the errors?
    } else {
      setReminders(response.reminders);
    }
  }

  async function createArbitraryReminder({ expiryDate, window, message }) {
    const context = await view.getContext();
    const issueSummary = await getIssueSummary(context);

    const expiry = moment(expiryDate).hour(window.value);

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

  const options = generateHoursOfDay().map((hourOfDay) => {
    return {
      label: hourOfDay.display,
      value: hourOfDay.index,
    };
  });

  const create = async (data) => {
    setAddReminderOpen(false);
    createArbitraryReminder(data);
  };

  return (
    <>
      <Heading as="h3">Add reminder</Heading>
      <Box xcss={buttonGroupBoxStyle}>
        <ButtonGroup>
          <Button onClick={() => createReminderForTomorrow()}>Tomorrow</Button>
          <Button onClick={() => createReminderForNextWeek()}>In a Week</Button>
          <Button onClick={() => setAddReminderOpen(true)}>
            Select a time...
          </Button>
        </ButtonGroup>
      </Box>
      {reminders && reminders.length > 0 && (
        <Box xcss={remindersSectionStyle}>
          <Heading as="h3">Your reminders</Heading>
          <Box xcss={remindersInnerStyle}>
            <Stack grow="fill" >
              {reminders.map(reminder => {
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
                      {message && (
                        <Text>{message || "<No Message>"}</Text>
                      )}
                    </Stack>
                  </Box>
                )
              })}
            </Stack>
          </Box>
        </Box>
      )}
      {reminders && reminders.length === 0 && (
        <SectionMessage>
          <Text>You have no reminders on this issue. Create some!</Text>
        </SectionMessage>
      )}
      {reminders === undefined && (
        <SectionMessage>
          <Text>Loading your reminders...</Text>
        </SectionMessage>
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
                {...register("expiryDate", { required: true })}
              />
              <HelperMessage>
                Set this date to the day that you want your reminder to be sent
              </HelperMessage>
              <Label labelFor="window">Hour of day</Label>
              <Select
                name="window"
                options={options}
                //TODO: add defaultValue hourOfDay.index === 5
                {...register("window")}
              />
              <HelperMessage>
                In which hour do you want your reminder to be sent?
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
