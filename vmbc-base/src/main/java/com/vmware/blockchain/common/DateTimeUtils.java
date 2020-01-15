/*
 * Copyright (c) 2019 VMware, Inc. All rights reserved. VMware Confidential
 */

package com.vmware.blockchain.common;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Calendar;
import java.util.Date;

/**
 * Utility class for DateTime mainly for logging purposes.
 * Be aware - DateTimeFormatter uses 'S' to mean 'fractions of a second' (so 'SSSSSS' means microseconds)
 * SimpleDateFormat uses 'S' to mean milliseconds (so 'SSSSSS' means left-zero filled milliseconds)
 * JsonFormat uses SimpleDateFormat - so currently (12/17/17) there is a bug where our API returns dates as:
 * 2017-12-17T18:31:40.000105Z instead of 2017-12-17T18:31:40.105000Z.
 * Since to fix this involves a subtle possible breaking change - introduce a new PATTERN for use in places that
 * need a SimpleDateFormat (which also means only millisecond resolution) - and work on converting all the API
 * usages.
 */
public class DateTimeUtils {

    public static final String ISO_8601_DATE_TIME_PATTERN = "yyyy-MM-dd'T'HH:mm:ss.SSSSSS'Z'";
    public static final String ISO_8601_SIMPLE_DATE_PATTERN = "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'";
    private static final DateTimeFormatter formatter = DateTimeFormatter.ofPattern(ISO_8601_DATE_TIME_PATTERN);
    private static final ZoneOffset zoneOffset = ZoneOffset.UTC;

    public static String getCurrentDateDisplayUtc() {
        return ZonedDateTime.now(zoneOffset).format(formatter);
    }

    /**
     * Helper method to get time difference between two ZonedDateTime.
     * This API should be called to get time difference for times created using
     * this class.
     * @param zonedDateTime1 time1.
     * @param zonedDateTime2 time2.
     * @return Time difference between time1 and time2 in milliseconds.
     */
    public static Long getTimeDifferenceInMilliSecs(String zonedDateTime1, String zonedDateTime2) {
        return Duration.between(ZonedDateTime.of(LocalDateTime.parse(zonedDateTime1, formatter), zoneOffset),
                ZonedDateTime.of(LocalDateTime.parse(zonedDateTime2, formatter), zoneOffset)).toMillis();
    }

    /**
     * Convert a string representation of date to a Calendar object.
     *
     * @param dateFormatString date format string, e.g. "yyyy-MM-dd HH:mm:ss"
     * @param dateString date in a String format
     * @return Calendar object representing the date
     */
    public static Calendar parseDate(String dateFormatString, String dateString) {
        DateFormat dateFormat = new SimpleDateFormat(dateFormatString);
        Date date;
        try {
            date = dateFormat.parse(dateString);
        } catch (ParseException ex) {
            throw new InternalFailureException(ex, ErrorCode.FAILED_TO_PARSE_DATE,
                    dateString, dateFormatString, ex.getMessage());
        }

        Calendar calendar = Calendar.getInstance();
        calendar.setTime(date);
        return calendar;
    }

}
