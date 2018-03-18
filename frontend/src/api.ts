import { PageContext } from './page-context';
import { DefaultApi } from './reminders-client';

export function createDefaultApi(pc: PageContext): DefaultApi {
    return new DefaultApi({
        apiKey: pc.acpt,
        basePath: '/rest'
    });
}