make_exported_helper_metadata_stub <- function() {
  list(
    table_id = "07459",
    table_label = "Test table",
    updated = "2024-01-01T00:00:00Z",
    full_table_estimated_rows = 4L,
    roles = list(
      geo = list("Region"),
      time = list("Tid"),
      metric = list("ContentsCode")
    ),
    dimensions = list(
      Region = list(
        label = "Region",
        codes = c("0301", "1103"),
        labels = c("Oslo", "Stavanger"),
        code_count = 2L,
        units = data.frame(
          code = character(0),
          unit_base = character(0),
          unit_decimals = integer(0),
          stringsAsFactors = FALSE
        ),
        codelists = list(
          list(
            id = "agg_KommSummer",
            label = "Summer municipalities",
            type = "Aggregation",
            href = "https://example.test/agg_KommSummer"
          )
        )
      ),
      Tid = list(
        label = "Tid",
        codes = c("2023", "2024"),
        labels = c("2023", "2024"),
        code_count = 2L,
        units = data.frame(
          code = character(0),
          unit_base = character(0),
          unit_decimals = integer(0),
          stringsAsFactors = FALSE
        ),
        codelists = list()
      ),
      ContentsCode = list(
        label = "Contents",
        codes = c("Folkemengde"),
        labels = c("Population"),
        code_count = 1L,
        units = data.frame(
          code = "Folkemengde",
          unit_base = "persons",
          unit_decimals = 0L,
          stringsAsFactors = FALSE
        ),
        codelists = list()
      )
    )
  )
}

run_with_exported_helper_mocks <- function(code, overrides = list()) {
  ns <- asNamespace("StatistikkbankR")
  replacement_names <- names(overrides)

  originals <- lapply(replacement_names, function(nm) get(nm, envir = ns, inherits = FALSE))
  names(originals) <- replacement_names
  was_locked <- vapply(replacement_names, function(nm) bindingIsLocked(nm, ns), logical(1))

  on.exit({
    for (nm in rev(replacement_names)) {
      if (bindingIsLocked(nm, ns)) {
        unlockBinding(nm, ns)
      }
      assign(nm, originals[[nm]], envir = ns)
      if (was_locked[[nm]]) {
        lockBinding(nm, ns)
      }
    }
  }, add = TRUE)

  for (nm in replacement_names) {
    if (bindingIsLocked(nm, ns)) {
      unlockBinding(nm, ns)
    }
    assign(nm, overrides[[nm]], envir = ns)
    if (was_locked[[nm]]) {
      lockBinding(nm, ns)
    }
  }

  eval(substitute(code), parent.frame())
}

test_that("ssb_codes returns codes and labels for a known dimension", {
  metadata <- make_exported_helper_metadata_stub()

  run_with_exported_helper_mocks(
    {
      out <- StatistikkbankR::ssb_codes("07459", "Region")
      expect_equal(out$table_id, c("07459", "07459"))
      expect_equal(out$dimension, c("Region", "Region"))
      expect_equal(out$code, c("0301", "1103"))
      expect_equal(out$label, c("Oslo", "Stavanger"))
    },
    overrides = list(
      .validate_table_request_args = function(...) NULL,
      .validate_metadata_cache_args = function(...) NULL,
      .get_table_metadata = function(...) metadata
    )
  )
})

test_that("ssb_codes rejects unknown dimensions", {
  metadata <- make_exported_helper_metadata_stub()

  run_with_exported_helper_mocks(
    {
      expect_error(
        StatistikkbankR::ssb_codes("07459", "Unknown"),
        "Unknown dimension"
      )
    },
    overrides = list(
      .validate_table_request_args = function(...) NULL,
      .validate_metadata_cache_args = function(...) NULL,
      .get_table_metadata = function(...) metadata
    )
  )
})

test_that("ssb_describe returns table summary and dimension roles", {
  metadata <- make_exported_helper_metadata_stub()

  run_with_exported_helper_mocks(
    {
      out <- StatistikkbankR::ssb_describe("07459")
      expect_equal(out$table$table_id, "07459")
      expect_equal(out$table$table_label, "Test table")
      expect_equal(out$dimensions$dimension, c("Region", "Tid", "ContentsCode"))
      expect_equal(out$dimensions$role, c("geo", "time", "metric"))
      expect_equal(out$dimensions$has_units, c(FALSE, FALSE, TRUE))
    },
    overrides = list(
      .validate_table_request_args = function(...) NULL,
      .validate_metadata_cache_args = function(...) NULL,
      .get_table_metadata = function(...) metadata
    )
  )
})

test_that("ssb_units returns flattened unit metadata and can filter by dimension", {
  metadata <- make_exported_helper_metadata_stub()

  run_with_exported_helper_mocks(
    {
      out <- StatistikkbankR::ssb_units("07459", dimension = "ContentsCode")
      expect_equal(nrow(out), 1L)
      expect_equal(out$dimension, "ContentsCode")
      expect_equal(out$code, "Folkemengde")
      expect_equal(out$unit_base, "persons")
    },
    overrides = list(
      .validate_table_request_args = function(...) NULL,
      .validate_metadata_cache_args = function(...) NULL,
      .get_table_metadata = function(...) metadata
    )
  )
})

test_that("ssb_codelists returns flattened codelist metadata", {
  metadata <- make_exported_helper_metadata_stub()

  run_with_exported_helper_mocks(
    {
      out <- StatistikkbankR::ssb_codelists("07459")
      expect_equal(nrow(out), 1L)
      expect_equal(out$dimension, "Region")
      expect_equal(out$codelist_id, "agg_KommSummer")
      expect_true(out$supports_output_values)
      expect_equal(out$suggested_output_values, "aggregated,single")
    },
    overrides = list(
      .validate_table_request_args = function(...) NULL,
      .validate_metadata_cache_args = function(...) NULL,
      .get_table_metadata = function(...) metadata
    )
  )
})

test_that("ssb_codelist_details returns tibble values when requested", {
  parsed_out <- list(
    codelist = list(id = "agg_KommSummer", label = "Summer municipalities"),
    values = data.frame(
      code = c("A", "B"),
      label = c("Group A", "Group B"),
      mapped_values = c("0301,0302", "1103"),
      mapped_count = c(2L, 1L),
      stringsAsFactors = FALSE
    )
  )

  run_with_exported_helper_mocks(
    {
      out <- StatistikkbankR::ssb_codelist_details(" agg_KommSummer ", as_tibble = TRUE)
      expect_equal(out$codelist$id, "agg_KommSummer")
      expect_s3_class(out$values, "tbl_df")
      expect_equal(out$values$code, c("A", "B"))
    },
    overrides = list(
      .build_codelist_request_spec = function(codelist_id, language) list(id = codelist_id, language = language),
      .execute_request_spec = function(request_spec) request_spec,
      .parse_response_json = function(resp) resp,
      .parse_codelist_response = function(parsed) parsed_out
    )
  )
})

test_that("ssb_expand_codelist_mapping expands members and preserves unmatched rows", {
  input <- data.frame(
    Region = c("A", "B", "Z"),
    value = c(10, 20, 30),
    stringsAsFactors = FALSE
  )

  mapping <- list(
    values = data.frame(
      code = c("A", "B"),
      label = c("Group A", "Group B"),
      mapped_values = c("0301,0302", "1103"),
      mapped_count = c(2L, 1L),
      stringsAsFactors = FALSE
    )
  )

  run_with_exported_helper_mocks(
    {
      out <- StatistikkbankR::ssb_expand_codelist_mapping(
        data = input,
        code_col = "Region",
        codelist_id = "agg_KommSummer",
        as_tibble = FALSE
      )
      expect_equal(out$Region, c("A", "A", "B", "Z"))
      expect_equal(out$member_code, c("0301", "0302", "1103", NA_character_))
      expect_equal(out$value, c(10, 10, 20, 30))
    },
    overrides = list(
      ssb_codelist_details = function(...) mapping
    )
  )
})

test_that("ssb_get_by_codelist forwards detected dimension and output value", {
  codelists_df <- data.frame(
    table_id = "07459",
    dimension = "Region",
    dimension_label = "Region",
    codelist_id = "agg_KommSummer",
    codelist_label = "Summer municipalities",
    codelist_type = "Aggregation",
    supports_output_values = TRUE,
    suggested_output_values = "aggregated,single",
    codelist_href = "https://example.test/agg_KommSummer",
    stringsAsFactors = FALSE
  )

  run_with_exported_helper_mocks(
    {
      out <- StatistikkbankR::ssb_get_by_codelist(
        "07459",
        "agg_KommSummer",
        Tid = "2024",
        output_value = "aggregated",
        as_tibble = FALSE
      )
      expect_equal(out$codelists$Region, "agg_KommSummer")
      expect_equal(out$output_values$Region, "aggregated")
      expect_equal(out$Tid, "2024")
    },
    overrides = list(
      .validate_table_request_args = function(...) NULL,
      .validate_metadata_cache_args = function(...) NULL,
      ssb_codelists = function(...) codelists_df,
      get_ssb_data = function(...) list(...)
    )
  )
})

test_that("ssb_get_by_codelist rejects ambiguous codelist matches without dimension", {
  codelists_df <- data.frame(
    table_id = c("07459", "07459"),
    dimension = c("Region", "Kjonn"),
    dimension_label = c("Region", "Kjonn"),
    codelist_id = c("agg_same", "agg_same"),
    codelist_label = c("Same", "Same"),
    codelist_type = c("Aggregation", "Aggregation"),
    supports_output_values = c(TRUE, TRUE),
    suggested_output_values = c("aggregated,single", "aggregated,single"),
    codelist_href = c("https://example.test/a", "https://example.test/b"),
    stringsAsFactors = FALSE
  )

  run_with_exported_helper_mocks(
    {
      expect_error(
        StatistikkbankR::ssb_get_by_codelist("07459", "agg_same"),
        "multiple dimensions"
      )
    },
    overrides = list(
      .validate_table_request_args = function(...) NULL,
      .validate_metadata_cache_args = function(...) NULL,
      ssb_codelists = function(...) codelists_df
    )
  )
})