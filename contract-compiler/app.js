/*
 * Copyright 2019 VMware, all rights reserved.
 */

const cookieParser = require('cookie-parser');
const express = require('express');
const httpErrors = require('http-errors');
const logger = require('morgan');
const bodyParser = require('body-parser');

const contractsRouter = require('./routes/contracts');

const app = express();

app.use('/static', express.static('public'));

app.use(logger('dev'));
app.use(bodyParser.json({ limit: '50mb' }));
app.use(bodyParser.urlencoded({ limit: '50mb', extended: true }));
app.use(cookieParser());

app.use('/api/v1/contracts', contractsRouter);

// catch 404 and forward to error handler
app.use((req, res, next) => {
  next(httpErrors(404));
});

// error handler
app.use((err, req, res, next) => {
  // set locals, only providing error in development
  res.locals.message = err.message;
  res.locals.error = req.app.get('env') === 'development' ? err : {};

  // render an error response
  res.status(err.status || 500);
  res.json({ error: err.message });
});

module.exports = app;
