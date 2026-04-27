import { deleteReminder } from '../resolvers/reminderPersistence';

// Mock @forge/kvs
const mockGet = jest.fn();
const mockDelete = jest.fn();
const mockQuery = jest.fn();

jest.mock('@forge/kvs', () => ({
  __esModule: true,
  default: {
    entity: () => ({
      get: mockGet,
      delete: mockDelete,
      query: () => ({
        index: () => ({
          sort: () => ({
            getMany: mockQuery,
            cursor: () => ({ getMany: mockQuery }),
          }),
        }),
      }),
    }),
  },
  Sort: { ASC: 'ASC' },
}));

beforeEach(() => {
  jest.clearAllMocks();
  jest.spyOn(console, 'error').mockImplementation(() => {});
});

afterEach(() => {
  console.error.mockRestore();
});

describe('deleteReminder', () => {
  it('deletes the reminder when the calling user owns it', async () => {
    const reminder = { userAaid: 'user-123', issueId: 1 };
    mockGet.mockResolvedValue(reminder);
    mockDelete.mockResolvedValue(undefined);

    await deleteReminder('reminder-key-1', 'user-123');

    expect(mockGet).toHaveBeenCalledWith('reminder-key-1');
    expect(mockDelete).toHaveBeenCalledWith('reminder-key-1');
  });

  it('does NOT delete when a different user tries to delete', async () => {
    const reminder = { userAaid: 'user-123', issueId: 1 };
    mockGet.mockResolvedValue(reminder);

    await deleteReminder('reminder-key-1', 'attacker-456');

    expect(mockDelete).not.toHaveBeenCalled();
    expect(console.error).toHaveBeenCalledWith(
      expect.stringContaining('SECURITY ALERT')
    );
  });

  it('logs an error when the reminder does not exist', async () => {
    mockGet.mockResolvedValue(null);

    await deleteReminder('nonexistent-key', 'user-123');

    expect(mockDelete).not.toHaveBeenCalled();
    expect(console.error).toHaveBeenCalledWith(
      expect.stringContaining('no longer exists')
    );
  });
});

describe('getYourReminders', () => {
  it('returns all reminders from a single page', async () => {
    const { getYourReminders } = await import('../resolvers/reminderPersistence');
    const fakeResults = [{ key: 'r1', value: {} }, { key: 'r2', value: {} }];
    mockQuery.mockResolvedValue({ results: fakeResults, nextCursor: null });

    const result = await getYourReminders({ userAaid: 'user-123' });

    expect(result).toEqual(fakeResults);
  });

  it('paginates through multiple pages', async () => {
    const { getYourReminders } = await import('../resolvers/reminderPersistence');
    const page1 = [{ key: 'r1', value: {} }];
    const page2 = [{ key: 'r2', value: {} }];
    mockQuery
      .mockResolvedValueOnce({ results: page1, nextCursor: 'cursor-1' })
      .mockResolvedValueOnce({ results: page2, nextCursor: null });

    const result = await getYourReminders({ userAaid: 'user-123' });

    expect(result).toEqual([...page1, ...page2]);
    expect(mockQuery).toHaveBeenCalledTimes(2);
  });
});
