#ifndef CONFIG_DEBUG_BUILD

#define _printf(...)
#else

int _printf(const char *restrict fmt, ...);

#endif /* CONFIG_DEBUG_BUILD */

#ifdef NDEBUG
#define assert(x) (void)0
#else
#define assert(x) ((void)((x) || (__assert_fail(#x, __FILE__, __LINE__, __func__),0)))
#endif

#define LOG_LEVEL_F 0
#define LOG_LEVEL_E 1
#define LOG_LEVEL_W 2
#define LOG_LEVEL_I 3
#define LOG_LEVEL_D 4
#define LOG_LEVEL_V 5

#define LOG_LEVEL LOG_LEVEL_V

#define LOG(lvl, ...) do {if (LOG_LEVEL >= lvl) {_printf(__VA_ARGS__); _printf("\n");}} while(0)
#define LOGF(...) LOG(LOG_LEVEL_F, __VA_ARGS__)
#define LOGE(...) LOG(LOG_LEVEL_E, __VA_ARGS__)
#define LOGW(...) LOG(LOG_LEVEL_W, __VA_ARGS__)
#define LOGI(...) LOG(LOG_LEVEL_I, __VA_ARGS__)
#define LOGD(...) LOG(LOG_LEVEL_D, __VA_ARGS__)
#define LOGV(...) LOG(LOG_LEVEL_V, __VA_ARGS__)


#define LOGE_IFERR(err, fmt, ...) \
	if ((err) != seL4_NoError) \
		{ LOGE("[Err %d]:\n\t" fmt, err, ## __VA_ARGS__); }

#define LOGD_IF(cond, fmt, ...) \
	if (cond) { LOGD("[Cond failed: %s]\n\t" fmt, #cond, ## __VA_ARGS__); }

static inline void ABORT_IF(int predicate, const char *const fmt, ...) {
    assert(!predicate);
}

static inline void ABORT_IFERR(int predicate, const char *const fmt, ...) {
    assert(predicate == seL4_NoError);
}

static inline void ABORT(const char *const fmt, ...) {
    assert(0);
}

