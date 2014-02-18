
/* <mpd/protocol.h> {{{ */

enum mpd_server_error {
	MPD_SERVER_ERROR_UNK = -1,

	MPD_SERVER_ERROR_NOT_LIST = 1,
	MPD_SERVER_ERROR_ARG = 2,
	MPD_SERVER_ERROR_PASSWORD = 3,
	MPD_SERVER_ERROR_PERMISSION = 4,
	MPD_SERVER_ERROR_UNKNOWN_CMD = 5,

	MPD_SERVER_ERROR_NO_EXIST = 50,
	MPD_SERVER_ERROR_PLAYLIST_MAX = 51,
	MPD_SERVER_ERROR_SYSTEM = 52,
	MPD_SERVER_ERROR_PLAYLIST_LOAD = 53,
	MPD_SERVER_ERROR_UPDATE_ALREADY = 54,
	MPD_SERVER_ERROR_PLAYER_SYNC = 55,
	MPD_SERVER_ERROR_EXIST = 56
};

/* <mpd/protocol.h> }}} */
/* <mpd/error.h> {{{ */

enum mpd_error {
	MPD_ERROR_SUCCESS = 0,
	MPD_ERROR_OOM,
	MPD_ERROR_ARGUMENT,
	MPD_ERROR_STATE,
	MPD_ERROR_TIMEOUT,
	MPD_ERROR_SYSTEM,
	MPD_ERROR_RESOLVER,
	MPD_ERROR_MALFORMED,
	MPD_ERROR_CLOSED,
	MPD_ERROR_SERVER
};

/* <mpd/error.h> }}} */
/* <mpd/settings.h> {{{ */

struct mpd_settings;

struct mpd_settings *
mpd_settings_new(const char *host, unsigned int port, unsigned int timeout_ms,
		const char *reserved, const char *password);

void
mpd_settings_free(struct mpd_settings *settings);

const char *
mpd_settings_get_host(const struct mpd_settings *settings);

unsigned int
mpd_settings_get_port(const struct mpd_settings *settings);

unsigned int
mpd_settings_get_timeout_ms(const struct mpd_settings *settings);

const char *
mpd_settings_get_password(const struct mpd_settings *settings);

/* <mpd/settings.h> }}} */
/* <mpd/connection.h>  {{{ */

struct mpd_connection;

struct mpd_connection *
mpd_connection_new(const char *host, unsigned int port, unsigned int timeout_ms);

void
mpd_connection_free(struct mpd_connection *connection);

const struct mpd_settings *
mpd_connection_get_settings(const struct mpd_connection *connection);

enum mpd_error
mpd_connection_get_error(const struct mpd_connection *connection);

const char *
mpd_connection_get_error_message(const struct mpd_connection *connection);

enum mpd_server_error
mpd_connection_get_server_error(const struct mpd_connection *connection);

int
mpd_connection_get_system_error(const struct mpd_connection *connection);

bool
mpd_connection_clear_error(struct mpd_connection *connection);

/* <mpd/connection.h> }}} */
/* <mpd/player.h> {{{ */

struct mpd_song;

struct mpd_song *
mpd_run_current_song(struct mpd_connection *connection);

bool
mpd_run_play(struct mpd_connection *connection);

bool
mpd_run_play_pos(struct mpd_connection *connection, unsigned int song_pos);

bool
mpd_run_play_id(struct mpd_connection *connection, unsigned int song_id);

bool
mpd_run_stop(struct mpd_connection *connection);

bool
mpd_run_toggle_pause(struct mpd_connection *connection);

bool
mpd_run_pause(struct mpd_connection *connection, bool mode);

bool
mpd_run_next(struct mpd_connection *connection);

bool
mpd_run_previous(struct mpd_connection *connection);

bool
mpd_run_seek_pos(struct mpd_connection *connection);

bool
mpd_run_seek_id(struct mpd_connection *connection);

bool
mpd_run_repeat(struct mpd_connection *connection, bool mode);

bool
mpd_run_random(struct mpd_connection *connection, bool mode);

bool
mpd_run_single(struct mpd_connection *connection, bool mode);

bool
mpd_run_consume(struct mpd_connection *connection, bool mode);

bool
mpd_run_crossfade(struct mpd_connection *connection, unsigned int seconds);

bool
mpd_run_mixrampdb(struct mpd_connection *connection, float db);

bool
mpd_run_mixrampdelay(struct mpd_connection *connection, float seconds);

bool
mpd_run_clearerror(struct mpd_connection *connection);

/* <mpd/player.h> }}} */
/* <mpd/status.h> {{{ */

enum mpd_state {
	MPD_STATE_UNKNOWN = 0,
	MPD_STATE_STOP = 1,
	MPD_STATE_PLAY = 2,
	MPD_STATE_PAUSE = 3
};

struct mpd_pair;
struct mpd_audio_format;
struct mpd_status;

struct mpd_status *
mpd_run_status(struct mpd_connection *connection);

void
mpd_status_free(struct mpd_status *status);

int
mpd_status_get_volume(const struct mpd_status *status);

bool
mpd_status_get_repeat(const struct mpd_status *status);

bool
mpd_status_get_random(const struct mpd_status *status);

bool
mpd_status_get_single(const struct mpd_status *status);

bool
mpd_status_get_consume(const struct mpd_status *status);

unsigned int
mpd_status_get_queue_length(const struct mpd_status *status);

unsigned int
mpd_status_get_queue_version(const struct mpd_status *status);

enum mpd_state
mpd_status_get_state(const struct mpd_status *status);

unsigned int
mpd_status_get_crossfade(const struct mpd_status *status);

float
mpd_status_get_mixrampdb(const struct mpd_status *status);

float
mpd_status_get_mixrampdelay(const struct mpd_status *status);

int
mpd_status_get_song_pos(const struct mpd_status *status);

int
mpd_status_get_song_id(const struct mpd_status *status);

int
mpd_status_get_next_song_pos(const struct mpd_status *status);

int
mpd_status_get_next_song_id(const struct mpd_status *status);

unsigned int
mpd_status_get_elapsed_time(const struct mpd_status *status);

unsigned int
mpd_status_get_elapsed_ms(const struct mpd_status *status);

unsigned int
mpd_status_get_total_time(const struct mpd_status *status);

unsigned int
mpd_status_get_kbit_rate(const struct mpd_status *status);

const struct mpd_audio_format *
mpd_status_get_audio_format(const struct mpd_status *status);

unsigned int
mpd_status_get_update_id(const struct mpd_status *status);

const char *
mpd_status_get_error(const struct mpd_status *status);

/* <mpd/status.h> }}} */
/* <mpd/stats.h> {{{ */

struct mpd_stats;

struct mpd_stats *
mpd_run_stats(struct mpd_connection *connection);

void
mpd_stats_free(struct mpd_stats *stats);

unsigned int
mpd_stats_get_number_of_artists(const struct mpd_stats *stats);

unsigned int
mpd_stats_get_number_of_albums(const struct mpd_stats *stats);

unsigned int
mpd_stats_get_number_of_songs(const struct mpd_stats *stats);

unsigned long
mpd_stats_get_uptime(const struct mpd_stats *stats);

unsigned long
mpd_stats_get_db_update_time(const struct mpd_stats *stats);

unsigned long
mpd_stats_get_play_time(const struct mpd_stats *stats);

unsigned long
mpd_stats_get_db_play_time(const struct mpd_stats *stats);

/* <mpd/stats.h> }}} */
/* <mpd/tag.h> {{{ */

enum mpd_tag_type
{
	MPD_TAG_UNKNOWN = -1,

	MPD_TAG_ARTIST,
	MPD_TAG_ALBUM,
	MPD_TAG_ALBUM_ARTIST,
	MPD_TAG_TITLE,
	MPD_TAG_TRACK,
	MPD_TAG_NAME,
	MPD_TAG_GENRE,
	MPD_TAG_DATE,
	MPD_TAG_COMPOSER,
	MPD_TAG_PERFORMER,
	MPD_TAG_COMMENT,
	MPD_TAG_DISC,

	MPD_TAG_MUSICBRAINZ_ARTISTID,
	MPD_TAG_MUSICBRAINZ_ALBUMID,
	MPD_TAG_MUSICBRAINZ_ALBUMARTISTID,
	MPD_TAG_MUSICBRAINZ_TRACKID,

	MPD_TAG_COUNT
};

const char *
mpd_tag_name(long type);

long
mpd_tag_name_parse(const char *name);

long
mpd_tag_name_iparse(const char *name);

/* <mpd/tag.h> }}} */
/* <mpd/song.h> {{{ */

struct mpd_song;

void
mpd_song_free(struct mpd_song *song);

const char *
mpd_song_get_uri(const struct mpd_song *song);

const char *
mpd_song_get_tag(const struct mpd_song *song,
		long type, unsigned int idx);

unsigned int
mpd_song_get_duration(const struct mpd_song *song);

unsigned int
mpd_song_get_start(const struct mpd_song *song);

unsigned int
mpd_song_get_end(const struct mpd_song *song);

unsigned long
mpd_song_get_last_modified(const struct mpd_song *song);

void
mpd_song_set_pos(struct mpd_song *song, unsigned int pos);

unsigned int
mpd_song_get_pos(const struct mpd_song *song);

unsigned int
mpd_song_get_id(const struct mpd_song *song);

unsigned int
mpd_song_get_prio(const struct mpd_song *song);

struct mpd_song *
mpd_song_begin(const struct mpd_pair *pair);

bool
mpd_song_feed(struct mpd_song *song, const struct mpd_pair *pair);

struct mpd_song *
mpd_recv_song(struct mpd_connection *connection);

/* <mpd/song.h> }}} */
