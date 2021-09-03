// Code to read Nicolet .spa MIR spectra files
//
// Based on code by Kurt Oldenburg - 06/28/16
// Adapted for use with Rcpp by David Benn - 03/09/2021
// NOTE: this code assumes that your platform has 32-bit IEEE floats and CHAR_BIT == 8

#include <Rcpp.h>
using namespace Rcpp;

uint8_t spa_ok=0;
uint8_t spa_open_error=1;
uint8_t spa_seek_error=2;
uint8_t spa_read_error=3;
uint8_t spa_alloc_error=4;

typedef union {
	uint8_t u8[2];
	uint16_t u16;
} endianness_test;

// Function prototypes

List parse_nicolet_spa(const char* filename);

List cleanup(float* intensities, FILE* fh, uint8_t result);

List spa_result(std::vector<float> wavelengths, std::vector<float> intensities,
                std::string comment, uint8_t result);

static size_t fread_le(void * ptr, size_t size, size_t nmemb, FILE* stream);

// [[Rcpp::export]]
List parse_nicolet_spa(const char* filename) {
  char comment[256];
  comment[0] = '\0';
  float* intensities = 0;
  std::vector<float> wavelengths_vec;
  std::vector<float> intensities_vec;
  std::size_t num_points;

	FILE* fh = fopen(filename, "rb");
	if (!fh) {
		return cleanup(intensities, fh, spa_open_error);
	}
	// do the comment
	if (fseek(fh,30,SEEK_SET)) {
		return cleanup(intensities, fh, spa_seek_error);
	}
	if (fread(comment, 255, 1, fh) != 1) { // no byte-swapping needed here
		return cleanup(intensities, fh, spa_read_error);
	}
	comment[255] = '\0';

	uint32_t local_num_points;
	// get number of points
	if (fseek(fh,564,SEEK_SET)) {
		return cleanup(intensities, fh, spa_seek_error);
	}
	if (fread_le(&local_num_points, sizeof(uint32_t), 1, fh) != 1) {
		return cleanup(intensities, fh, spa_read_error);
	}
	// hopefully, your either your platform is 64-bit or you don't store 2**32 spectra in the first place
	num_points = local_num_points;

	// get max,min wavenumber
	float wn_maxmin[2];
	if (fseek(fh,576,SEEK_SET)) {
		return cleanup(intensities, fh, spa_seek_error);
	}
	if (fread_le(wn_maxmin, sizeof(float), 2, fh) != 2) {
		return cleanup(intensities, fh, spa_read_error);
	}

	// locate the data
	uint16_t flag=0, offset=0;
	if (fseek(fh,288,SEEK_SET)) {
		return cleanup(intensities, fh, spa_seek_error);
	}
	do {
		if (fread_le(&flag, sizeof(uint16_t), 1, fh) != 1) {
			return cleanup(intensities, fh, spa_read_error);
		}
	} while (flag != 3);
	if (fread_le(&offset, sizeof(uint16_t), 1, fh) != 1) {
		return cleanup(intensities, fh, spa_read_error);
	}

	// read output spectral data
	intensities = static_cast<float*>(calloc(num_points,sizeof(float)));
	if (!intensities) {
		return cleanup(intensities, fh, spa_alloc_error);
	}
	if (fseek(fh,offset,SEEK_SET)) {
		return cleanup(intensities, fh, spa_seek_error);
	}
	if (fread(intensities, sizeof(float), num_points, fh) != num_points) {
		return cleanup(intensities, fh, spa_read_error);
	}
	for (size_t i = 0; i < num_points; i++) {
		wavelengths_vec.push_back(wn_maxmin[0] - (wn_maxmin[0]-wn_maxmin[1])*i / num_points);
	    intensities_vec.push_back(intensities[i]);
	}

    if (fh != 0) fclose(fh);
    if (intensities != 0) free(intensities);

	return spa_result(wavelengths_vec, intensities_vec, comment, spa_ok);
}

// byte swapping will occur for each `size`-sized item
static size_t fread_le(void * ptr, size_t size, size_t nmemb, FILE* stream) {
	{
		size_t ret = 0;
		if ((ret = fread(ptr, size, nmemb, stream)) != nmemb) return ret;
	}

	if (((endianness_test){.u16 = 0x0100}).u8[0]) { // host is big-endian => must byteswap
		char item[size]; // yes, this is C99 VLA

		for (size_t i = 0; i < nmemb; i++) {
			std::memcpy(item, (char*)ptr + i*size, size);
			for (size_t j = 0; j < size; j++)
				*((char*)ptr + i*size + j) = item[size-j-1];
		}
	}

	return nmemb;
}

List cleanup(float* intensities, FILE* fh, uint8_t result) {
    if (fh != 0) fclose(fh);
    if (intensities != 0) free(intensities);

    return spa_result(std::vector<float>(), std::vector<float>(), "", result);
}

List spa_result(std::vector<float> wavelengths, std::vector<float> intensities,
                std::string comment, uint8_t result) {

    return List::create(
	  _["wavelengths"] = wavelengths,
	  _["intensities"] = intensities,
    _["num_points"] = wavelengths.size(),
	  _["comment"] = comment,
	  _["status"] = result
	);
}
