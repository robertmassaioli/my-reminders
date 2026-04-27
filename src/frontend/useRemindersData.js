import { useState } from "react";
import { invoke, view } from "@forge/bridge";
import moment from 'moment-timezone/builds/moment-timezone-with-data';
import { isPresent } from "ts-is-present";
import { useEffectAsync } from "../useEffectAsync";
import { getSiteInfo } from "./siteInfo";

/**
 * Shared hook that fetches reminders, site info, and user timezone.
 * Used by both viewYourReminders and dashboardGadget to avoid duplicated
 * data-fetching logic across surfaces.
 *
 * @returns {{ allReminders, setAllReminders, siteInfo, userTimezone }}
 */
export function useRemindersData() {
  const [allReminders, setAllReminders] = useState(undefined);
  const [siteInfo, setSiteInfo] = useState(undefined);
  const [userTimezone, setUserTimezone] = useState(undefined);

  useEffectAsync(async () => {
    setSiteInfo(await getSiteInfo());
  }, []);

  useEffectAsync(async () => {
    const response = await invoke("getYourReminders");
    setAllReminders(response.reminders);
  }, []);

  useEffectAsync(async () => {
    const context = await view.getContext();
    setUserTimezone(context.timezone);
  }, []);

  if (isPresent(userTimezone)) {
    moment.tz.setDefault(userTimezone);
  }

  return { allReminders, setAllReminders, siteInfo, userTimezone };
}
