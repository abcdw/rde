/**
 * Claude Subscription Status Extension
 *
 * Adds /cstatus command to display Claude Pro/Max subscription usage limits:
 * session (5-hour) and weekly (7-day) utilization with reset times.
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { homedir } from "node:os";

const AUTH_PATH = join(homedir(), ".pi", "agent", "auth.json");
const USAGE_ENDPOINT = "https://api.anthropic.com/api/oauth/usage";
const BETA_HEADER = "oauth-2025-04-20";

interface UsageWindow {
	utilization: number;
	resets_at: string;
}

interface UsageResponse {
	five_hour: UsageWindow | null;
	seven_day: UsageWindow | null;
	seven_day_oauth_apps: UsageWindow | null;
	seven_day_opus: UsageWindow | null;
	seven_day_sonnet: UsageWindow | null;
	seven_day_cowork: UsageWindow | null;
	iguana_necktie: UsageWindow | null;
	extra_usage: {
		is_enabled: boolean;
		monthly_limit: number | null;
		used_credits: number | null;
		utilization: number | null;
	} | null;
}

function getAccessToken(): string | null {
	try {
		const raw = readFileSync(AUTH_PATH, "utf-8");
		const auth = JSON.parse(raw);
		return auth?.anthropic?.access ?? null;
	} catch {
		return null;
	}
}

function formatResetTime(isoString: string): string {
	const reset = new Date(isoString);
	const now = new Date();
	const diffMs = reset.getTime() - now.getTime();

	const time = reset.toLocaleTimeString(undefined, {
		hour: "2-digit",
		minute: "2-digit",
		timeZoneName: "short",
	});
	const date = reset.toLocaleDateString(undefined, {
		month: "short",
		day: "numeric",
	});

	if (diffMs <= 0) return `${date} ${time} (now)`;

	const diffMin = Math.floor(diffMs / 60000);
	const hours = Math.floor(diffMin / 60);
	const mins = diffMin % 60;

	const parts: string[] = [];
	if (hours > 0) parts.push(`${hours}h`);
	if (mins > 0) parts.push(`${mins}m`);

	return `${date} ${time} (in ${parts.join(" ")})`;
}

function formatBar(pct: number, width: number = 20): string {
	const filled = Math.round((pct / 100) * width);
	const empty = width - filled;
	return "[" + "#".repeat(filled) + ".".repeat(empty) + "]";
}

function formatWindow(label: string, window: UsageWindow | null): string | null {
	if (!window) return null;
	const bar = formatBar(window.utilization);
	const reset = formatResetTime(window.resets_at);
	return `${label}: ${bar} ${window.utilization}%  resets ${reset}`;
}

function formatUsage(data: UsageResponse): string {
	const lines: string[] = [];
	lines.push("-- Claude Subscription Usage --");
	lines.push("");

	const windows: [string, UsageWindow | null][] = [
		["Session (5h)", data.five_hour],
		["Weekly  (7d)", data.seven_day],
		["Weekly Opus", data.seven_day_opus],
		["Weekly Sonnet", data.seven_day_sonnet],
		["Weekly Cowork", data.seven_day_cowork],
		["OAuth Apps", data.seven_day_oauth_apps],
	];

	for (const [label, window] of windows) {
		const line = formatWindow(label, window);
		if (line) lines.push(line);
	}

	if (data.extra_usage) {
		lines.push("");
		if (data.extra_usage.is_enabled) {
			const used = data.extra_usage.used_credits ?? 0;
			const limit = data.extra_usage.monthly_limit ?? 0;
			const pct = data.extra_usage.utilization ?? 0;
			lines.push(`Extra usage: ${formatBar(pct)} ${pct}% ($${used}/$${limit})`);
		} else {
			lines.push("Extra usage: not enabled");
		}
	}

	return lines.join("\n");
}

export default function (pi: ExtensionAPI) {
	pi.registerCommand("cstatus", {
		description: "Show Claude subscription usage limits",
		handler: async (_args, ctx) => {
			const token = getAccessToken();
			if (!token) {
				ctx.ui.notify(`Cannot read OAuth token from ${AUTH_PATH}`, "error");
				return;
			}

			try {
				const response = await fetch(USAGE_ENDPOINT, {
					method: "GET",
					headers: {
						Authorization: `Bearer ${token}`,
						"Content-Type": "application/json",
						"anthropic-beta": BETA_HEADER,
					},
				});

				if (!response.ok) {
					const body = await response.text().catch(() => "");
					ctx.ui.notify(`API error ${response.status}: ${body}`, "error");
					return;
				}

				const data = (await response.json()) as UsageResponse;
				ctx.ui.notify(formatUsage(data), "info");
			} catch (err) {
				const msg = err instanceof Error ? err.message : String(err);
				ctx.ui.notify(`Failed to fetch usage: ${msg}`, "error");
			}
		},
	});
}
