-- ===================================================================
-- PostgreSQL LISTEN/NOTIFY Trigger for Workflow Status Updates
-- ===================================================================
--
-- This script creates a PostgreSQL trigger that sends real-time
-- notifications when workflow_runs table is updated.
--
-- Installation:
--   psql -U your_db_user -d metaboanalyst -f workflow_notifications_trigger.sql
--
-- Testing:
--   -- In one terminal (listening):
--   LISTEN workflow_status_testuser;
--
--   -- In another terminal (trigger update):
--   UPDATE workflow_runs SET status='completed' WHERE id=1;
--
--   -- First terminal should receive notification
--
-- Uninstall:
--   DROP TRIGGER IF EXISTS workflow_status_trigger ON workflow_runs;
--   DROP FUNCTION IF EXISTS notify_workflow_status_change();
--
-- ===================================================================

-- Drop existing trigger and function if they exist (for clean reinstall)
DROP TRIGGER IF EXISTS workflow_status_trigger ON workflow_runs;
DROP FUNCTION IF EXISTS notify_workflow_status_change();

-- Create function to send notifications
CREATE OR REPLACE FUNCTION notify_workflow_status_change()
RETURNS TRIGGER AS $$
DECLARE
    notification json;
    channel_name text;
    sanitized_email text;
BEGIN
    -- Sanitize email for channel name (PostgreSQL identifiers rules)
    -- Convert to lowercase and replace non-alphanumeric with underscore
    sanitized_email := lower(regexp_replace(NEW.email, '[^a-z0-9]', '_', 'g'));

    -- Build channel name: workflow_status_{sanitized_email}
    channel_name := 'workflow_status_' || sanitized_email;

    -- Build JSON payload with workflow status information
    notification = json_build_object(
        'runId', NEW.id,
        'userId', NEW.email,
        'status', NEW.status,
        'projectId', NEW.project_id,
        'lastUpdated', EXTRACT(EPOCH FROM COALESCE(NEW.last_updated, NOW()))::bigint,
        'startDate', NEW.start_date,
        'finishDate', NEW.finish_date
    );

    -- Send notification to channel
    -- This is extremely lightweight - no disk I/O, pure in-memory operation
    PERFORM pg_notify(channel_name, notification::text);

    -- Log notification for debugging (optional - can be removed in production)
    -- RAISE NOTICE 'Sent notification to channel %: %', channel_name, notification;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Add comment explaining the function
COMMENT ON FUNCTION notify_workflow_status_change() IS
    'Sends PostgreSQL notification when workflow_runs status or project_id changes. ' ||
    'Notifications are received by WorkflowNotificationService and pushed to browser clients via SSE.';

-- Create trigger on workflow_runs table
CREATE TRIGGER workflow_status_trigger
AFTER UPDATE OF status, project_id ON workflow_runs
FOR EACH ROW
WHEN (
    -- Only trigger when status or project_id actually changes
    OLD.status IS DISTINCT FROM NEW.status OR
    OLD.project_id IS DISTINCT FROM NEW.project_id
)
EXECUTE FUNCTION notify_workflow_status_change();

-- Add comment explaining the trigger
COMMENT ON TRIGGER workflow_status_trigger ON workflow_runs IS
    'Triggers notification when workflow status or project_id changes. ' ||
    'Enables real-time status updates without polling.';

-- ===================================================================
-- Verification Queries
-- ===================================================================
--
-- 1. Check if trigger exists:
--    SELECT * FROM pg_trigger WHERE tgname = 'workflow_status_trigger';
--
-- 2. Check if function exists:
--    SELECT * FROM pg_proc WHERE proname = 'notify_workflow_status_change';
--
-- 3. Test notification manually:
--    -- Terminal 1:
--    LISTEN workflow_status_testuser;
--
--    -- Terminal 2:
--    SELECT pg_notify('workflow_status_testuser',
--                     '{"runId":999,"status":"test"}');
--
--    -- Terminal 1 should receive: Asynchronous notification "workflow_status_testuser" received...
--
-- ===================================================================

-- Grant necessary permissions (adjust if needed)
-- GRANT EXECUTE ON FUNCTION notify_workflow_status_change() TO your_app_user;
